package scala.meta.internal.metals

import java.{util => ju}

import scala.util.control.NonFatal

import scala.meta.dialects
import scala.meta.internal.metals.Configs.DefinitionProviderConfig
import scala.meta.internal.metals.Configs.ProtobufLspConfig
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtWorkspaceSymbolProvider
import scala.meta.internal.metals.mbt.ProtoEvidence
import scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles
import scala.meta.internal.metals.mbt.ProtoJavaSymbolMapper
import scala.meta.internal.metals.mbt.ProtoJavaVirtualFile
import scala.meta.internal.metals.mbt.ProtoOrigin
import scala.meta.internal.metals.mbt.ProtoOutputMapping
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.proto.ProtoDeclaration
import scala.meta.internal.mtags.proto.ProtoLayout
import scala.meta.internal.mtags.proto.ProtoNaming
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

  /**
   * Which `.proto` a generated file or symbol came from, and the other way
   * round. Every question of that shape goes through it so that one set of rules
   * answers them all.
   */
  private def mapping: ProtoOutputMapping = mbt.protoOutputMapping

  def hasProtoJavaLocation(res: DefinitionResult): Boolean =
    if (!protobufLspConfig().definition) false
    else {
      val it = res.locations.iterator()
      var found = false
      while (!found && it.hasNext) {
        found = mapping.isSynthesizedOutline(it.next().getUri())
      }
      // A proto-generated class can also be resolved without any location at
      // all, unlike the Java PC's virtual SOURCE_PATH entry which already
      // points at the outline. So also treat a locationless result as a
      // proto-Java hit if it matches a known outline.
      found ||
      (res.locations.isEmpty && mbt.protoJavaOutlineFor(res.symbol).isDefined)
    }

  /**
   * Adds what the header comment of the generated file names: the proto, and the
   * proto's other outputs that declare the same symbol.
   *
   * A generator writing `// source: a/b/model.proto` states the origin outright,
   * which beats anything [[withProtoOrigin]] can infer from the symbol's shape --
   * so this runs first, and that one stands down once a proto is in the list. It
   * therefore has to offer the sibling outputs too, or a banner would cost the
   * user every entry but one: `java_multiple_files` declares a service in both
   * `SvcGrpc.java` and `SvcImplBase.java`, and both carry the header.
   */
  def enhanceWithProtobufDefinition(
      result: DefinitionResult
  ): DefinitionResult = try {
    if (!definitionProviders().isProtobuf) {
      return result
    }
    val alreadyPointsAtProto =
      result.locations.asScala.exists(_.getUri().isProtoFilename)
    val extra =
      if (alreadyPointsAtProto) Nil
      else {
        val origins = for {
          path <- result.definition.toList
          if !mapping.isSynthesizedOutline(path.toURI.toString())
          origin <- mapping.originsOfFile(path)
          if origin.evidence == ProtoEvidence.Banner
        } yield origin
        // The generated code keeps the leading slots -- it is what compiles --
        // and the proto explains where it came from. Outlines are left out on
        // purpose: a real source is in the result by construction here, so an
        // outline would only be a stub copy of an entry already listed.
        distinctByUri(
          origins.flatMap(realSourceLocationsOf(_, result.symbol)) ++
            origins.flatMap(protoDeclarationLocations(_, result.symbol))
        )
      }
    appendProtoLocations(result, extra)
  } catch {
    case NonFatal(e) =>
      scribe.warn(
        s"failed to enhance with protobuf definition for '${result.symbol}'",
        e,
      )
      result
  }

  /**
   * `result` with `extra` appended, skipping any file it already offers.
   *
   * Deduplicated by file, not by range: the same file offered twice with
   * near-identical ranges is noise in the dropdown.
   */
  private def appendProtoLocations(
      result: DefinitionResult,
      extra: Seq[Location],
  ): DefinitionResult = {
    val knownUris = result.locations.asScala.map(_.getUri()).toSet
    val added =
      extra.filterNot(location => knownUris.contains(location.getUri()))
    if (added.isEmpty) result
    else {
      val allLocations =
        new ju.ArrayList[Location](result.locations.size() + added.size)
      allLocations.addAll(result.locations)
      added.foreach(allLocations.add)
      result.copy(locations = allLocations)
    }
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
        outlineDuplicatesRealSource && isSynthesizedOutline(location)
      )
      appendProtoLocations(result, extra)
    }
  }

  private def isSynthesizedOutline(location: Location): Boolean =
    mapping.isSynthesizedOutline(location.getUri())

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
    val origins = protoOrigins(res)
    // Every matching output, not the first one: `java_multiple_files` splits a
    // service across `Svc.java` and `SvcImplBase.java`, and declares an accessor
    // on both a message and its `OrBuilder` interface, so collapsing to a single
    // pick drops answers that are just as real. Ambiguous protos each contribute
    // their own entry too, rather than one being guessed at.
    val realSourceLocations =
      distinctByUri(origins.flatMap(realSourceLocationsOf(_, res.symbol)))
    val outlineLocations =
      distinctByUri(origins.flatMap(outlineLocationsOf(_, res.symbol)))
    val protoLocations =
      distinctByUri(origins.flatMap(protoDeclarationLocations(_, res.symbol)))

    val allLocations =
      realSourceLocations ++ outlineLocations ++ protoLocations
    if (allLocations.nonEmpty) {
      Some(
        DefinitionResult(
          allLocations.asJava,
          res.symbol,
          // Recording the materialized file as the destination lets Metals
          // remember the build target jumped from
          // (InteractiveSemanticdbs.didDefinition), so later requests inside it
          // use that target's classpath, which carries the protobuf runtime.
          // Still a single path, whatever the dropdown holds.
          definition = outlineLocations.headOption
            .orElse(realSourceLocations.headOption)
            .map(_.getUri().toAbsolutePath),
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
   * The protos `res` could have been generated from, most likely first.
   *
   * A file the compiler already pointed at states its origin outright -- an
   * outline carries the proto's path in its own, a generated file may name it in
   * a header comment -- so the symbol's shape is only read when no location says
   * anything, and never to second-guess one that does.
   */
  private def protoOrigins(res: DefinitionResult): Seq[ProtoOrigin] = {
    val fromLocations = for {
      location <- res.locations.asScala.toSeq
      file <- location.getUri().toAbsolutePathSafe.toSeq
      origin <- mapping.originsOfFile(file)
      if origin.evidence == ProtoEvidence.OutlinePath ||
        origin.evidence == ProtoEvidence.Banner
    } yield origin
    if (fromLocations.nonEmpty) fromLocations.distinctBy(_.proto)
    else mapping.originsOfSymbol(res.symbol)
  }

  /**
   * The generated files on disk that declare `symbol`, all of them.
   *
   * Asked of the proto rather than read off the origin, because an origin reached
   * through a path or a header comment knows only the one file it came from,
   * while the proto has as many outputs declaring the symbol as its layout
   * implies.
   */
  private def realSourceLocationsOf(
      origin: ProtoOrigin,
      symbol: String,
  ): Seq[Location] =
    for {
      output <- mapping.outputsDeclaring(origin.proto, symbol)
      uri = output.file.toURI.toString()
      if !mapping.isSynthesizedOutline(uri)
      range <- findJavaSymbolRange(output.file, symbol).toSeq
    } yield new Location(uri, range.toLsp)

  /**
   * The outlines Metals synthesized from the proto that declare `symbol`,
   * materialized to read-only files so the client can open them.
   *
   * Derived purely from the `.proto`, so this works with no build and no
   * configuration -- and stands in for generated code that was never written or
   * has been cleaned away.
   */
  private def outlineLocationsOf(
      origin: ProtoOrigin,
      symbol: String,
  ): Seq[Location] =
    try {
      for {
        outline <- mapping.outlinesDeclaring(origin.proto, symbol)
        className <- ProtoJavaVirtualFile
          .extractClassName(outline.uri().toString())
          .toSeq
        javaFile <- ProtoGeneratedJavaFiles
          .materialize(workspace, origin.proto, className, outline.text)
          .toSeq
        range <- findJavaSymbolRange(javaFile, symbol).toSeq
      } yield new Location(javaFile.toURI.toString(), range.toLsp)
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"proto-java: failed to resolve generated Java outline for $symbol",
          e,
        )
        Nil
    }

  /**
   * Where the `.proto` declares what `symbol` was generated from: the message,
   * enum or service it names, else the field or rpc it accesses.
   */
  private def protoDeclarationLocations(
      origin: ProtoOrigin,
      symbol: String,
  ): Seq[Location] = {
    val declaration =
      origin.declaration.orElse(mapping.declarationOf(origin.proto, symbol))
    findProtoDeclaration(origin.proto, symbol, declaration)
      .orElse(findProtoFieldFromJavaMethod(origin.proto, symbol))
      .orElse(findProtoRpcFromJavaMethod(origin.proto, symbol))
      .toSeq
  }

  private def distinctByUri(locations: Seq[Location]): Seq[Location] =
    locations.distinctBy(_.getUri())

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
   * The declaration whose owner chain corresponds to `querySymbol`'s, for a
   * symbol that names the same type as the outline but nests it differently: the
   * query `com/example/api/jproto/model/User#` is `List("User")` after its
   * package and finds the outline's `com/example/api/jproto/Model#User#`, which
   * is `List("Model", "User")` -- one chain ends with the other.
   */
  private def findRangeByOwnerChain(
      querySymbol: Symbol,
      definitions: Iterable[SymbolOccurrence],
  ): Option[s.Range] = {
    val ownerChain = ProtoLayout.nameChainAfterPackage(querySymbol)
    if (ownerChain.isEmpty) None
    else
      definitions
        .find(occurrence =>
          ProtoLayout.nameChainsCorrespond(
            ownerChain,
            ProtoLayout.nameChainAfterPackage(Symbol(occurrence.symbol)),
          )
        )
        .flatMap(_.range)
  }

  /**
   * Where `declaration` is written in the `.proto`, when `javaSymbol` names it
   * rather than one of its members.
   *
   * The innermost name has to be the declaration's, up to the suffix a generator
   * appended -- `UserOrBuilder` and `GreeterGrpc` name `User` and `Greeter`,
   * while `User#getName().` names a member of `User` and belongs to the field
   * lookup instead. Without that test every accessor would resolve to its
   * enclosing message.
   */
  private def findProtoDeclaration(
      protoPath: AbsolutePath,
      javaSymbol: String,
      declaration: Option[ProtoDeclaration],
  ): Option[Location] = {
    import scala.meta.internal.mtags.proto.ProtoMtagsV2

    try {
      // A ScalaPB companion's `apply` is the message itself, not a member of it.
      val querySymbol = Symbol(javaSymbol.stripSuffix("apply()."))
      val innermostName =
        ProtoLayout.nameChainAfterPackage(querySymbol).lastOption
      for {
        declared <- declaration
        name <- innermostName
        if ProtoNaming.declarationNameCandidates(name).contains(declared.name)
        input = protoPath.toInputFromBuffers(buffers)
        document = new ProtoMtagsV2(input, includeMembers = false).index()
        occurrence <- document.occurrences.find(occurrence =>
          occurrence.symbol == declared.symbol &&
            occurrence.role == s.SymbolOccurrence.Role.DEFINITION
        )
        range <- occurrence.range
      } yield new Location(protoPath.toURI.toString(), range.toLsp)
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
      val ownerChain =
        ProtoLayout.nameChainAfterPackage(Symbol(javaSymbol).owner)
      val fieldNames = extractMethodName(javaSymbol).toSeq
        .flatMap(ProtoNaming.protoFieldNameCandidates)

      if (fieldNames.isEmpty) None
      else {
        val input = protoPath.toInputFromBuffers(buffers)
        val document = new ProtoMtagsV2(input, includeMembers = true).index()
        val fieldSuffixes = fieldNames.map(fieldName => s"$fieldName().")

        // A field is placed by the message owning it, so the chains have to
        // line up -- except each side nests what the other does not: the
        // generated side adds `Builder` (`List("User", "Builder")`) or an outer
        // class, the proto side the enclosing `oneof` (`List("User", "contact")`).
        // So the strict pass runs over the whole file first, then the relaxed
        // one that lets the proto sit deeper.
        def protoNestsDeeper(
            javaChain: List[String],
            protoChain: List[String],
        ) =
          javaChain.tails.exists(suffix =>
            ProtoLayout.nameChainEncloses(suffix, protoChain)
          )

        def fieldDeclaredIn(
            fieldSuffix: String,
            javaChain: List[String],
            matches: (List[String], List[String]) => Boolean,
        ) =
          document.occurrences.find { occurrence =>
            occurrence.symbol.endsWith(fieldSuffix) &&
            occurrence.role == s.SymbolOccurrence.Role.DEFINITION &&
            (javaChain.isEmpty || matches(
              javaChain,
              ProtoLayout.nameChainAfterPackage(Symbol(occurrence.symbol).owner),
            ))
          }

        // Longest chain first: a field on a nested message beats a same-named
        // field on its parent. The shorter prefixes then absorb the levels the
        // generated side added.
        val chains =
          if (ownerChain.isEmpty) Iterator(List.empty[String])
          else ownerChain.inits.filter(_.nonEmpty)

        // Candidate field names in order too: inverting an accessor name is
        // lossy, so `getFooBarList` reads as both `foo_bar` and `foo_bar_list`
        // and only the proto says which it is.
        val found = for {
          chain <- chains
          fieldSuffix <- fieldSuffixes.iterator
          occurrence <- fieldDeclaredIn(
            fieldSuffix,
            chain,
            ProtoLayout.nameChainsCorrespond,
          ).orElse(fieldDeclaredIn(fieldSuffix, chain, protoNestsDeeper))
        } yield occurrence

        for {
          occurrence <- found.nextOption()
          range <- occurrence.range
        } yield new Location(protoPath.toURI.toString(), range.toLsp)
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
        val rpcName = ProtoNaming.capitalize(methodName)

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

}
