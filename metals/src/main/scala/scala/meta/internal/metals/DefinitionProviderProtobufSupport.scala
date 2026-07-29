package scala.meta.internal.metals

import java.{util => ju}

import scala.util.control.NonFatal

import scala.meta.dialects
import scala.meta.internal.jmbt.Mbt
import scala.meta.internal.metals.Configs.DefinitionProviderConfig
import scala.meta.internal.metals.Configs.ProtobufLspConfig
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtWorkspaceSymbolProvider
import scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles
import scala.meta.internal.metals.mbt.ProtoJavaSymbolMapper
import scala.meta.internal.metals.mbt.ProtoJavaVirtualFile
import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.Location

final class DefinitionProviderProtobufSupport(
    workspace: AbsolutePath,
    buffers: Buffers,
    mbt: MbtWorkspaceSymbolProvider,
    definitionProviders: () => DefinitionProviderConfig,
    protobufLspConfig: () => ProtobufLspConfig,
    mtags: () => Mtags,
) {

  def hasProtoJavaLocation(res: DefinitionResult): Boolean =
    if (!protobufLspConfig().definition) false
    else {
      val it = res.locations.iterator()
      var found = false
      while (!found && it.hasNext) {
        found = ProtoJavaVirtualFile.isProtoJavaUri(it.next().getUri())
      }
      // A proto-generated class can also be resolved without any location at
      // all, unlike the Java PC's virtual SOURCE_PATH entry which already
      // points at the outline. So also treat a locationless result as a
      // proto-Java hit if it matches a known outline.
      found ||
      (res.locations.isEmpty && mbt.protoJavaOutlineFor(res.symbol).isDefined)
    }

  def enhanceWithProtobufDefinition(
      result: DefinitionResult
  ): DefinitionResult = try {
    if (!definitionProviders().isProtobuf) {
      return result
    }
    val protoLocation: Option[Location] = (for {
      path <- result.definition.iterator
      if !ProtoJavaVirtualFile.isProtoJavaUri(path.toURI.toString())
      source <- path
        .toInputFromBuffers(buffers)
        .text
        .linesIterator
        .takeWhile(line => line.startsWith("//") || line.startsWith(" //"))
        .collectFirst {
          case s"// source: $source" => source
          case s" // testing-only-source: $source" => source
        }
        .iterator
      protoDoc <- mbt.document(workspace.resolve(source)).iterator
      sym = Symbol(result.symbol.stripSuffix("apply()."))
      symSuffix = symbolSuffixAfterPackage(sym)
      protoSym <- selectBestProtoSymbol(
        sym,
        protoDoc.symbols.iterator.filter { protoSym =>
          val protoSymSuffix =
            symbolSuffixAfterPackage(Symbol(protoSym.getSymbol()))
          symbolSuffixMatches(symSuffix, protoSymSuffix)
        },
      ).iterator
    } yield new Location(
      protoDoc.file.toURI.toString(),
      protoSym.getDefinitionRange().toLspRange,
    )).headOption

    protoLocation.fold(result) { loc =>
      // Appended, not prepended: the generated source is what the code
      // compiles against, so it keeps the first slot. [[withProtoOrigin]] has
      // to agree, since all that decides which route fires is whether the
      // generated file carries a `// source:` header.
      val allLocations = new ju.ArrayList[Location](result.locations.size() + 1)
      allLocations.addAll(result.locations)
      allLocations.add(loc)
      result.copy(locations = allLocations)
    }
  } catch {
    case NonFatal(e) =>
      scribe.warn(
        s"failed to enhance with protobuf definition for '${result.symbol}'",
        e,
      )
      result
  }

  /**
   * Where a proto-generated symbol is declared, for a compiler that reported
   * it without any location of its own.
   *
   * Empty when the symbol is not proto-generated, which is the common case:
   * callers use this only once their own lookup has come up empty.
   */
  def protoDefinitionLocations(symbol: String): List[Location] =
    protoOrigin(symbol).toList.flatMap(_.locations.asScala)

  private def protoOrigin(symbol: String): Option[DefinitionResult] =
    if (!protobufLspConfig().definition) None
    else protoOriginResult(DefinitionResult.empty(symbol)).filterNot(_.isEmpty)

  /**
   * Adds the proto a generated symbol came from, and the outline synthesized
   * from it, to a result that has no proto location yet.
   *
   * Locations already found stay first -- they are what the code compiles
   * against, and the proto only explains where they came from. With none (the
   * generator never ran, or its output was cleaned away) the proto is all
   * there is to offer.
   *
   * The outline is dropped once a real source is in the list, being a stub
   * duplicate of it.
   */
  def withProtoOrigin(result: DefinitionResult): DefinitionResult = {
    val symbol =
      if (result.symbol.nonEmpty) result.symbol else result.querySymbol
    val alreadyPointsAtProto =
      result.locations.asScala.exists(_.getUri().isProtoFilename)
    if (
      !protobufLspConfig().definition || symbol.isEmpty ||
      symbol.endsWith("/") || alreadyPointsAtProto
    ) result
    else if (result.isEmpty) protoOrigin(symbol).getOrElse(result)
    else {
      // Deduplicated by file, not by range: the same file offered twice with
      // near-identical ranges is noise in the dropdown.
      val knownUris = result.locations.asScala.map(_.getUri()).toSet
      // Against a real source the outline is a subset: what the `.proto`
      // implies and nothing the generator added on top. So the test is where
      // the result points, not whether the outline's name lines up with the
      // symbol -- a generator that nests its output in a per-file sub-package
      // (`api/jproto/model/User#` against the outline's `api/jproto/Model#User#`)
      // never lines up, and its output is no less real.
      val outlineDuplicatesRealSource =
        result.locations.asScala.exists(location =>
          !isSynthesizedOutline(location)
        )
      val extra = protoDefinitionLocations(symbol).filterNot(location =>
        knownUris.contains(location.getUri()) ||
          (outlineDuplicatesRealSource && isSynthesizedOutline(location))
      )
      if (extra.isEmpty) result
      else {
        val allLocations = new ju.ArrayList[Location](result.locations)
        extra.foreach(allLocations.add)
        result.copy(locations = allLocations)
      }
    }
  }

  /**
   * Whether `location` points at an outline Metals synthesized from a `.proto`
   * -- either the virtual entry the Java compiler resolves against,
   * `file:///a/model.proto.metals-proto-java/User.java`, or the file
   * materialized for the client to open,
   * `.metals/readonly/dependencies/proto-generated/a/model.proto/User.java`.
   */
  private def isSynthesizedOutline(location: Location): Boolean =
    ProtoJavaVirtualFile.isProtoJavaUri(location.getUri()) ||
      location
        .getUri()
        .toAbsolutePathSafe
        .flatMap(ProtoGeneratedJavaFiles.protoPathFor(workspace, _))
        .isDefined

  /**
   * The proto file and synthesized outline that `symbol` was generated from,
   * for code a generator wrote to disk and that therefore carries no virtual
   * proto URI of its own.
   *
   * Only what every generator agrees on is matched: the type sits under the
   * package the proto configures and keeps the name the proto declares. The
   * nesting in between is what they disagree on, so it is not checked --
   * `com/example/api/jproto/model/User#` and `com/example/api/jproto/Model#User#`
   * both resolve to `message User` in `model.proto`.
   */
  private def generatedFromProto(
      symbol: String
  ): Option[VirtualTextDocument] = {
    val querySymbol = Symbol(symbol)
    val ownerChain = symbolSuffixAfterPackage(querySymbol)
    if (ownerChain.isEmpty) None
    else
      for {
        (protoPath, anyOutline) <- mbt
          .protoJavaOutlinesUnderPackage(querySymbol.enclosingPackage.value)
          .find { case (protoPath, _) => declaresOwner(protoPath, ownerChain) }
      } yield {
        // `java_multiple_files` yields one outline per class, so taking any of
        // them would as often land on a sibling. A single-file proto nests
        // everything in an outer class that no chain matches, and there the
        // one outline is the answer.
        mbt
          .protoJavaOutlines(protoPath)
          .find(outline =>
            outline
              .toplevelSymbols()
              .asScala
              .exists(toplevelSymbol =>
                symbolSuffixMatches(
                  ownerChain,
                  symbolSuffixAfterPackage(Symbol(toplevelSymbol)),
                )
              )
          )
          .getOrElse(anyOutline)
      }
  }

  /**
   * Whether the proto declares a type matching `ownerChain` or one of its
   * enclosing prefixes -- a field or a method is placed by its owner, so
   * `List("user", "getname")` is satisfied by a proto declaring `message User`.
   */
  private def declaresOwner(
      protoPath: AbsolutePath,
      ownerChain: List[String],
  ): Boolean = {
    val protoTypes = for {
      document <- mbt.document(protoPath).toList
      symbolInfo <- document.symbols
      protoSymbol = Symbol(symbolInfo.getSymbol())
      if protoSymbol.isType
    } yield symbolSuffixAfterPackage(protoSymbol)
    ownerChain.inits.exists(chain =>
      chain.nonEmpty && protoTypes.exists(symbolSuffixMatches(chain, _))
    )
  }

  def handleProtoJavaDefinition(
      res: DefinitionResult
  ): Option[DefinitionResult] = {
    // Without a `java_package` option the generated Java package equals the
    // proto package, so the MBT index answers the Java symbol with the proto
    // document itself. Dropping those hits keeps the outline lookup running;
    // the proto stays reachable through the fallback below.
    val mbtJavaResult =
      mbt.definition(res.symbol).filterNot(_.getUri.isProtoFilename)
    if (mbtJavaResult.nonEmpty) {
      val locations = new ju.ArrayList[Location](mbtJavaResult.size)
      mbtJavaResult.foreach(locations.add)
      Some(
        DefinitionResult(
          locations,
          res.symbol,
          None,
          None,
          res.querySymbol,
        )
      )
    } else protoOriginResult(res)
  }

  /**
   * The proto `res.symbol` was generated from, and the outline synthesized
   * from it.
   *
   * Never answers with the generated code itself, unlike
   * [[handleProtoJavaDefinition]], so a caller that already has that code can
   * append these instead of being handed back its own result.
   */
  private def protoOriginResult(
      res: DefinitionResult
  ): Option[DefinitionResult] = {
    val generatedJavaFileUri =
      res.locations.asScala.headOption
        .map(_.getUri())
        // No location at all (see [[hasProtoJavaLocation]]) -- look the
        // outline up by symbol instead of via a virtual URI.
        .orElse(mbt.protoJavaOutlineFor(res.symbol).map(_.uri().toString()))
        // Code a generator wrote to disk has no virtual URI, so the proto has
        // to come from the symbol's shape instead.
        .orElse(generatedFromProto(res.symbol).map(_.uri().toString()))
    val protoFilePath =
      generatedJavaFileUri.flatMap(ProtoJavaVirtualFile.extractProtoPath)

    val generatedJavaLocation = for {
      javaPath <- generatedJavaFileUri
      protoPath <- protoFilePath
      location <- findSymbolInGeneratedJavaFile(protoPath, res.symbol, javaPath)
    } yield location

    val protoClassLocation = protoFilePath.flatMap { protoPath =>
      findProtoClassFromJavaSymbol(protoPath, res.symbol)
    }

    val protoFieldLocation = protoClassLocation.orElse {
      protoFilePath.flatMap { protoPath =>
        findProtoFieldFromJavaMethod(protoPath, res.symbol)
      }
    }

    val protoRpcLocation = protoFieldLocation.orElse {
      protoFilePath.flatMap { protoPath =>
        findProtoRpcFromJavaMethod(protoPath, res.symbol)
      }
    }

    val allLocations = (generatedJavaLocation ++ protoRpcLocation).toList
    if (allLocations.nonEmpty) {
      Some(
        DefinitionResult(
          allLocations.asJava,
          res.symbol,
          // Recording the materialized file as the destination lets Metals
          // remember the build target jumped from
          // (InteractiveSemanticdbs.didDefinition), so later requests inside it
          // use that target's classpath, which carries the protobuf runtime.
          definition = generatedJavaLocation.map(_.getUri().toAbsolutePath),
          semanticdb = None,
          res.querySymbol,
        )
      )
    } else {
      scribe.debug(
        s"proto-java: could not resolve symbol ${res.symbol} to proto"
      )
      Some(DefinitionResult.empty)
    }
  }

  /**
   * Finds the definition of `javaSymbol` in the Java source that Metals
   * synthesizes from the proto file. The outline is derived purely from the
   * `.proto`, so this works for any build tool without a build or any
   * configuration; it is materialized to a read-only file on disk so the
   * client can open it.
   */
  private def findSymbolInGeneratedJavaFile(
      protoPath: AbsolutePath,
      javaSymbol: String,
      virtualUri: String,
  ): Option[Location] = try {
    for {
      className <- ProtoJavaVirtualFile.extractClassName(virtualUri)
      outline <- mbt
        .protoJavaOutlines(protoPath)
        .find(outline =>
          ProtoJavaVirtualFile
            .extractClassName(outline.uri().toString())
            .contains(className)
        )
      javaFile <- ProtoGeneratedJavaFiles.materialize(
        workspace,
        protoPath,
        className,
        outline.text,
      )
      range <- findJavaSymbolRange(javaFile, javaSymbol)
    } yield new Location(javaFile.toURI.toString(), range.toLsp)
  } catch {
    case NonFatal(e) =>
      scribe.debug(
        s"proto-java: failed to resolve generated Java outline for $javaSymbol",
        e,
      )
      None
  }

  /**
   * Locates the definition range of `javaSymbol` in the given Java file,
   * falling back to enclosing classes when the exact symbol is absent, for
   * example when it refers to an inherited member not present in the
   * synthesized outline.
   */
  private def findJavaSymbolRange(
      javaFile: AbsolutePath,
      javaSymbol: String,
  ): Option[s.Range] = {
    val input = javaFile.toInputFromBuffers(buffers)
    val doc = mtags().allToplevels(input, dialects.Scala213)
    val definitions =
      doc.occurrences.filter(_.role == s.SymbolOccurrence.Role.DEFINITION)

    def loop(sym: Symbol): Option[s.Range] = {
      if (sym.isNone || sym.isPackage) None
      else findRange(sym.value, definitions).orElse(loop(sym.owner))
    }

    val querySym = Symbol(javaSymbol)
    // Exact first, and for a type *only* the type itself: a message missing
    // from the outline means the outline is stale or belongs to another proto,
    // and an approximate enclosing-class range is worse than what the
    // owner-chain pass finds. Members may fall back to their enclosing class,
    // since inherited ones are often absent from the outline.
    val exact =
      if (querySym.isType) findRange(querySym.value, definitions)
      else loop(querySym)
    exact.orElse(findRangeByOwnerChain(querySym, definitions))
  }

  private def findRange(
      symbol: String,
      definitions: Iterable[SymbolOccurrence],
  ): Option[s.Range] =
    definitions.find(_.symbol == symbol).flatMap(_.range)

  /**
   * The declaration whose owner chain matches `querySymbol`'s, for a symbol
   * that names the same type as the outline but nests it differently: the query
   * `com/example/api/jproto/model/User#` is `List("user")` after its package
   * and finds the outline's `com/example/api/jproto/Model#User#`, which is
   * `List("model", "user")` -- one chain ends with the other.
   */
  private def findRangeByOwnerChain(
      querySymbol: Symbol,
      definitions: Iterable[SymbolOccurrence],
  ): Option[s.Range] = {
    val ownerChain = symbolSuffixAfterPackage(querySymbol)
    if (ownerChain.isEmpty) None
    else
      definitions
        .find(occurrence =>
          symbolSuffixMatches(
            ownerChain,
            symbolSuffixAfterPackage(Symbol(occurrence.symbol)),
          )
        )
        .flatMap(_.range)
  }

  /**
   * The lowercased chain of owner names between `sym` and its enclosing
   * package, for example the SemanticDB symbol
   * `com/example/Foo#Bar#baz().` with package `com.example` becomes
   * `List("foo", "bar", "baz")`.
   */
  private def symbolSuffixAfterPackage(sym: Symbol): List[String] = {
    val pkg = sym.enclosingPackage
    def loop(s: Symbol, acc: List[String]): List[String] = {
      if (s.isNone || s.isRootPackage || s.isEmptyPackage || s == pkg) acc
      else loop(s.owner, s.displayName.toLowerCase :: acc)
    }
    loop(sym, Nil)
  }

  /**
   * Whether either chain is a suffix of the other, so `List("model", "user")`
   * matches `List("user")` but not `List("user", "getname")`.
   */
  private def symbolSuffixMatches(
      lhs: List[String],
      rhs: List[String],
  ): Boolean = {
    lhs == rhs || endsWithSuffix(lhs, rhs) || endsWithSuffix(rhs, lhs)
  }

  private def endsWithSuffix(
      full: List[String],
      suffix: List[String],
  ): Boolean = {
    suffix.nonEmpty &&
    full.lengthCompare(suffix.length) >= 0 &&
    full.takeRight(suffix.length) == suffix
  }

  /**
   * Proto symbol suffix matching can be ambiguous (for example, `exampleType`
   * may match both enum type `ExampleType` and generated field accessor
   * `exampleType`). Prefer the symbol whose kind and display name best match
   * the original query symbol.
   */
  private def selectBestProtoSymbol(
      querySym: Symbol,
      candidates: Iterator[Mbt.SymbolInformation],
  ): Option[Mbt.SymbolInformation] = {
    val all = candidates.toList
    if (all.isEmpty) None
    else {
      val queryName = normalizeName(querySym.displayName)
      Some(all.maxBy { info =>
        val candidate = Symbol(info.getSymbol())
        val kindMatches =
          (querySym.isMethod && candidate.isMethod) ||
            (querySym.isType && candidate.isType)
        val exactNameMatch = candidate.displayName == querySym.displayName
        val normalizedNameMatch =
          normalizeName(candidate.displayName) == queryName
        (
          if (kindMatches) 1 else 0,
          if (exactNameMatch) 1 else 0,
          if (normalizedNameMatch) 1 else 0,
        )
      })
    }
  }

  private def normalizeName(name: String): String =
    name.replace("_", "").toLowerCase(ju.Locale.ROOT)

  private def findProtoClassFromJavaSymbol(
      protoPath: AbsolutePath,
      javaSymbol: String,
  ): Option[Location] = {
    import scala.meta.internal.mtags.proto.ProtoMtagsV2

    try {
      val sym = Symbol(javaSymbol)
      if (!sym.isType) return None

      val className = sym.displayName
      val input = protoPath.toInputFromBuffers(buffers)
      val protoMtags = new ProtoMtagsV2(input, includeMembers = false)
      val doc = protoMtags.index()

      // Candidate class-name suffixes to look for in proto symbols:
      //  1. The exact Java class name (messages, enums, services)
      //  2. The name with "Grpc" stripped — protoc-java wraps each gRPC service
      //     in an outer class called XxxGrpc, but ProtoMtagsV2 emits the bare
      //     service name (e.g. CompatibilityService#, not CompatibilityServiceGrpc#).
      val candidates = Seq(s"$className#") ++
        (if (className.endsWith("Grpc"))
           Seq(s"${className.dropRight(4)}#")
         else Nil)

      doc.occurrences
        .find { occ =>
          candidates.exists(occ.symbol.endsWith) &&
          occ.role == s.SymbolOccurrence.Role.DEFINITION
        }
        .flatMap { occ =>
          occ.range.map { range =>
            new Location(
              protoPath.toURI.toString(),
              range.toLsp,
            )
          }
        }
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"Failed to map Java class to proto type: $javaSymbol",
          e,
        )
        None
    }
  }

  private def findProtoFieldFromJavaMethod(
      protoPath: AbsolutePath,
      javaSymbol: String,
  ): Option[Location] = {
    import scala.meta.internal.mtags.proto.ProtoMtagsV2

    try {
      val methodNameOpt = extractMethodName(javaSymbol)
      val ownerChain = symbolSuffixAfterPackage(Symbol(javaSymbol).owner)

      methodNameOpt.flatMap(javaMethodToProtoField).flatMap { protoFieldName =>
        val input = protoPath.toInputFromBuffers(buffers)
        val protoJavaMtags = new ProtoMtagsV2(input, includeMembers = true)
        val doc = protoJavaMtags.index()
        val fieldSuffix = s"$protoFieldName()."

        // A field is placed by the message owning it, so the chains have to
        // line up -- except each side nests what the other does not: the
        // generated side adds `Builder` (`List("user", "builder")`) or an outer
        // class, the proto side the enclosing `oneof` (`List("user", "contact")`).
        // So the strict pass runs over the whole file first, then the relaxed
        // one that lets the proto sit deeper.
        def strictly(javaChain: List[String], protoChain: List[String]) =
          symbolSuffixMatches(javaChain, protoChain)

        def protoNestsDeeper(
            javaChain: List[String],
            protoChain: List[String],
        ) =
          javaChain.tails
            .exists(suffix => suffix.nonEmpty && protoChain.startsWith(suffix))

        def fieldDeclaredIn(
            javaChain: List[String],
            matches: (List[String], List[String]) => Boolean,
        ) =
          doc.occurrences.find { occ =>
            occ.symbol.endsWith(fieldSuffix) &&
            occ.role == s.SymbolOccurrence.Role.DEFINITION &&
            (javaChain.isEmpty || matches(
              javaChain,
              symbolSuffixAfterPackage(Symbol(occ.symbol).owner),
            ))
          }

        // Longest chain first: a field on a nested message beats a same-named
        // field on its parent. The shorter prefixes then absorb the levels the
        // generated side added.
        val chains =
          if (ownerChain.isEmpty) Iterator(List.empty[String])
          else ownerChain.inits.filter(_.nonEmpty)

        chains
          .flatMap(chain =>
            fieldDeclaredIn(chain, strictly)
              .orElse(fieldDeclaredIn(chain, protoNestsDeeper))
          )
          .nextOption()
          .flatMap { occ =>
            occ.range.map { range =>
              new Location(
                protoPath.toURI.toString(),
                range.toLsp,
              )
            }
          }
      }
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"Failed to map Java method to proto field: $javaSymbol",
          e,
        )
        None
    }
  }

  private def findProtoRpcFromJavaMethod(
      protoPath: AbsolutePath,
      javaSymbol: String,
  ): Option[Location] = {
    import scala.meta.internal.mtags.proto.ProtoMtagsV2

    try {
      if (!ProtoJavaSymbolMapper.isGrpcStubMethodSymbol(javaSymbol)) return None

      val methodNameOpt = extractMethodName(javaSymbol)

      methodNameOpt.flatMap { methodName =>
        // Java stub methods use the lowerCamel RPC name (e.g. `doSomething`),
        // while the proto symbol uses the UpperCamel RPC name declared in the
        // service (e.g. `DoSomething`).
        val rpcName = capitalize(methodName)

        val input = protoPath.toInputFromBuffers(buffers)
        val protoMtags = new ProtoMtagsV2(input, includeMembers = true)
        val doc = protoMtags.index()
        val rpcSuffix = s"$rpcName()."

        doc.occurrences
          .find { occ =>
            val matchesRpc = occ.symbol.endsWith(rpcSuffix)
            val isDefinition = occ.role == s.SymbolOccurrence.Role.DEFINITION
            val ownerSymbol = Symbol(occ.symbol).owner.value
            val isServiceMethod = doc.symbols.exists { info =>
              info.symbol == ownerSymbol && info.kind.isInterface
            }
            matchesRpc && isDefinition && isServiceMethod
          }
          .flatMap { occ =>
            occ.range.map { range =>
              new Location(
                protoPath.toURI.toString(),
                range.toLsp,
              )
            }
          }
      }
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"Failed to map Java gRPC method to proto RPC: $javaSymbol",
          e,
        )
        None
    }
  }

  private def capitalize(s: String): String = {
    if (s.isEmpty) s
    else s.head.toUpper + s.tail
  }

  /**
   * The method name of `symbol`, for example the SemanticDB symbol
   * `com/example/Foo#bar(+1).` becomes `bar`.
   */
  private def extractMethodName(symbol: String): Option[String] = {
    val hashIndex = symbol.lastIndexOf('#')
    if (hashIndex < 0) return None

    val afterHash = symbol.substring(hashIndex + 1)
    val parenIndex = afterHash.indexOf('(')
    if (parenIndex < 0) {
      Some(afterHash.stripSuffix("."))
    } else {
      Some(afterHash.substring(0, parenIndex))
    }
  }

  /**
   * The proto field name accessed by a generated Java accessor method, for
   * example:
   *   - `getFooBar` -> `foo_bar`
   *   - `setFooBarList` -> `foo_bar` (repeated field)
   *   - `hasFooBar` / `clearFooBar` -> `foo_bar`
   *   - `getHTTPCode` -> `HTTP_CODE` (already-uppercase names, e.g. proto
   *     enum-like fields, are kept as-is instead of snake-cased)
   *   - `fooBar` -> `foo_bar` (a generator that exposes the field directly)
   *
   * Inverse of [[scala.meta.internal.metals.mbt.ProtoJavaSymbolMapper#protoFieldToJavaMethods]].
   */
  private def javaMethodToProtoField(methodName: String): Option[String] = {
    val fieldName =
      if (methodName.startsWith("set") && methodName.length > 3)
        Some(methodName.substring(3))
      else if (methodName.startsWith("get") && methodName.length > 3) {
        val rest = methodName.substring(3)
        if (rest.endsWith("List")) Some(rest.stripSuffix("List"))
        else if (rest.endsWith("Map")) Some(rest.stripSuffix("Map"))
        else if (rest.endsWith("Count")) Some(rest.stripSuffix("Count"))
        else if (rest.endsWith("OrDefault")) Some(rest.stripSuffix("OrDefault"))
        else if (rest.endsWith("OrThrow")) Some(rest.stripSuffix("OrThrow"))
        else if (rest.endsWith("Bytes")) Some(rest.stripSuffix("Bytes"))
        else if (rest.endsWith("Builder")) Some(rest.stripSuffix("Builder"))
        else Some(rest)
      } else if (methodName.startsWith("has") && methodName.length > 3)
        Some(methodName.substring(3))
      else if (methodName.startsWith("clear") && methodName.length > 5)
        Some(methodName.substring(5))
      else if (methodName.startsWith("add") && methodName.length > 3) {
        val rest = methodName.substring(3)
        if (rest.startsWith("All") && rest.length > 3) Some(rest.substring(3))
        else Some(rest)
      } else if (methodName.startsWith("put") && methodName.length > 3) {
        val rest = methodName.substring(3)
        if (rest.startsWith("All") && rest.length > 3) Some(rest.substring(3))
        else Some(rest)
      } else if (methodName.startsWith("remove") && methodName.length > 6)
        Some(methodName.substring(6))
      else if (methodName.startsWith("contains") && methodName.length > 8)
        Some(methodName.substring(8))
      else if (methodName.forall(c => c.isUpper || c == '_' || c.isDigit))
        Some(methodName)
      else
        // Not every generator prefixes its accessors -- some expose the field
        // under its own name, so `fooBar` is the field itself.
        Some(methodName)

    fieldName.map { name =>
      if (name.forall(c => c.isUpper || c == '_' || c.isDigit)) name
      else camelToSnakeCase(name)
    }
  }

  private def camelToSnakeCase(camelCase: String): String = {
    if (camelCase.isEmpty) return camelCase
    val sb = new StringBuilder
    sb.append(camelCase.charAt(0).toLower)
    for (i <- 1 until camelCase.length) {
      val c = camelCase.charAt(i)
      if (c.isUpper) {
        sb.append('_')
        sb.append(c.toLower)
      } else {
        sb.append(c)
      }
    }
    sb.toString
  }
}
