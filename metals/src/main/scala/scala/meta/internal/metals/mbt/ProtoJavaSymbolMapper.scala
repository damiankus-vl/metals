package scala.meta.internal.metals.mbt

import scala.collection.mutable
import scala.util.control.NonFatal

import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.proto.ProtoLayout
import scala.meta.internal.mtags.proto.ProtoMtagsV2
import scala.meta.internal.mtags.proto.ProtoNaming
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

/**
 * Maps proto symbols to the generated Java symbols that could refer to them.
 *
 * This is what lets find-references and find-implementations start from a
 * `.proto` and reach the Java code compiled against its output. Both directions
 * of the naming rules live in [[ProtoNaming]], so this cannot drift from the
 * mapping that reads generated code back to a proto.
 *
 * Candidates, not answers: the field's type and cardinality are not known here,
 * and neither is the layout the code on disk was generated with, so every shape
 * a generator could have produced is emitted. The consumer validates them --
 * `symbolMapping.keySet` feeds a bloom-filter query, which needs concrete
 * strings and discards the misses.
 *
 * For example proto symbol `com/example/User#name().` with `java_package`
 * "com.example.jproto" maps to `com/example/jproto/User#getName().`,
 * `…#hasName().`, `…/User#Builder#setName().`, and so on.
 */
object ProtoJavaSymbolMapper {

  /**
   * Result of mapping proto symbols to Java symbols.
   *
   * @param symbolMapping Map from Java symbol to original proto symbol
   * @param protoPackage The proto package from the proto file
   * @param javaPackage The java_package from the proto file
   * @param javaAccessorMethods List of Java accessor method names to query for
   * @param methodNamePatterns Patterns to match in method chains (for unresolved symbol chains)
   */
  case class ProtoToJavaResult(
      symbolMapping: Map[String, String],
      protoPackage: String,
      javaPackage: String,
      javaAccessorMethods: Seq[String],
      methodNamePatterns: Seq[(String, String)] = Seq.empty,
  )

  object ProtoToJavaResult {
    val empty: ProtoToJavaResult =
      ProtoToJavaResult(Map.empty, "", "", Seq.empty)
  }

  /**
   * Maps proto symbols to their corresponding Java symbols.
   *
   * Uses ProtoMtagsV2 to parse the proto file and leverage its existing
   * knowledge of proto-to-Java symbol mapping.
   */
  def protoToJavaSymbols(
      protoPath: AbsolutePath,
      protoSymbols: Seq[String],
      buffers: Buffers,
  ): ProtoToJavaResult = {
    try {
      val input = protoPath.toInputFromBuffers(buffers)
      val protoMtags = new ProtoMtagsV2(input, includeMembers = true)
      val protoDocument = protoMtags.index()
      val layout = protoMtags.layout

      val symbolMapping = mutable.Map.empty[String, String]
      val accessorMethods = mutable.ListBuffer.empty[String]

      for (protoSymbol <- protoSymbols) {
        val symbol = Symbol(protoSymbol)
        // Only `INTERFACE` reliably means something in a proto document: it is
        // what distinguishes a service, and therefore an rpc, from a message and
        // its fields. Enums index as `CLASS` and oneofs as `PACKAGE_OBJECT`.
        def isService(candidate: String) =
          protoDocument.symbols.exists(info =>
            info.symbol == candidate && info.kind.isInterface
          )

        if (symbol.isType) {
          if (isService(protoSymbol))
            mapServiceSymbol(protoSymbol, symbol, layout, symbolMapping)
          else
            mapTypeSymbol(
              protoSymbol,
              symbol,
              layout,
              protoDocument,
              symbolMapping,
            )
        } else if (
          symbol.isMethod || (symbol.isTerm && !symbol.owner.isPackage)
        ) {
          if (isService(symbol.owner.value))
            mapRpcSymbol(
              protoSymbol,
              symbol,
              layout,
              symbolMapping,
              accessorMethods,
            )
          else
            mapFieldSymbol(
              protoSymbol,
              symbol,
              layout,
              protoDocument,
              symbolMapping,
              accessorMethods,
            )
        } else if (symbol.isTerm && symbol.owner.isPackage) {
          addSymbolMapping(
            symbolMapping,
            javaSymbolOf(protoSymbol, layout),
            protoSymbol,
            layout,
          )
        }
      }

      ProtoToJavaResult(
        symbolMapping.toMap,
        layout.protoPackage,
        layout.javaPackage,
        accessorMethods.toSeq,
      )
    } catch {
      case NonFatal(e) =>
        scribe.debug(s"Failed to map proto symbols to Java: ${e.getMessage}")
        ProtoToJavaResult.empty
    }
  }

  /**
   * Maps a proto service symbol to its Java gRPC class equivalents.
   *
   * A service becomes an outer class `SvcGrpc` holding `SvcImplBase`, `SvcStub`,
   * `SvcBlockingStub` and `SvcFutureStub`. Metals' own outline puts `SvcImplBase`
   * at the top level instead, so both nestings are emitted -- generated at one
   * shape and searched for at the other, find-implementations would miss.
   */
  private def mapServiceSymbol(
      protoSymbol: String,
      symbol: Symbol,
      layout: ProtoLayout,
      result: mutable.Map[String, String],
  ): Unit = {
    val serviceName = symbol.displayName
    val packageSymbol = javaPackageSymbol(layout)

    addSymbolMapping(
      result,
      javaSymbolOf(protoSymbol, layout),
      protoSymbol,
      layout,
    )

    val grpcClassSymbol =
      Symbols.Global(packageSymbol, Descriptor.Type(s"${serviceName}Grpc"))
    addSymbolMapping(result, grpcClassSymbol, protoSymbol, layout)

    for (stubName <- stubClassNames(serviceName)) {
      addSymbolMapping(
        result,
        Symbols.Global(grpcClassSymbol, Descriptor.Type(stubName)),
        protoSymbol,
        layout,
      )
      addSymbolMapping(
        result,
        Symbols.Global(packageSymbol, Descriptor.Type(stubName)),
        protoSymbol,
        layout,
      )
    }
  }

  /**
   * Maps a proto RPC symbol to its Java gRPC method equivalents.
   *
   * The method is declared on every stub class, under both the nested and the
   * top-level shape, and additionally without its package: that is what javac
   * reports for a class it could not resolve, which is exactly the case this
   * mapping exists to cover.
   */
  private def mapRpcSymbol(
      protoSymbol: String,
      symbol: Symbol,
      layout: ProtoLayout,
      result: mutable.Map[String, String],
      accessorMethods: mutable.ListBuffer[String],
  ): Unit = {
    val javaMethodName = ProtoNaming.decapitalize(symbol.displayName)
    val serviceName = symbol.owner.displayName
    val packageSymbol = javaPackageSymbol(layout)
    val grpcClassSymbol =
      Symbols.Global(packageSymbol, Descriptor.Type(s"${serviceName}Grpc"))

    for (stubName <- stubClassNames(serviceName)) {
      val qualifiedStubs = Seq(
        Symbols.Global(grpcClassSymbol, Descriptor.Type(stubName)),
        Symbols.Global(packageSymbol, Descriptor.Type(stubName)),
      ).distinct
      val bareStub = Symbols.Global(Symbols.None, Descriptor.Type(stubName))

      for (stubSymbol <- qualifiedStubs) {
        addSymbolMapping(
          result,
          Symbols.Global(stubSymbol, Descriptor.Method(javaMethodName, "()")),
          protoSymbol,
          layout,
        )
        // The stub class also maps to the rpc, so that find-references can tell
        // an override of it from an unrelated method of the same name.
        addSymbolMapping(result, stubSymbol, protoSymbol, layout)
      }
      for (
        descriptor <- Seq(
          Descriptor.Method(javaMethodName, "()"),
          Descriptor.Type(javaMethodName),
        )
      )
        addSymbolMapping(
          result,
          Symbols.Global(bareStub, descriptor),
          protoSymbol,
          layout,
        )
    }

    accessorMethods += javaMethodName
  }

  private def stubClassNames(serviceName: String): Seq[String] =
    ProtoNaming.grpcStubSuffixes.map(suffix => serviceName + suffix)

  def isGrpcStubClassSymbol(symbol: String): Boolean =
    ProtoNaming.isGrpcStubName(Symbol(symbol).displayName)

  def isGrpcStubMethodSymbol(symbol: String): Boolean = {
    val parsed = Symbol(symbol)
    parsed.isMethod && isGrpcStubClassSymbol(parsed.owner.value)
  }

  /**
   * Maps a proto type symbol (message or enum) to its Java equivalent, plus
   * every Java-shaped name the proto document itself records for it.
   */
  private def mapTypeSymbol(
      protoSymbol: String,
      symbol: Symbol,
      layout: ProtoLayout,
      protoDocument: s.TextDocument,
      result: mutable.Map[String, String],
  ): Unit = {
    addSymbolMapping(
      result,
      javaSymbolOf(protoSymbol, layout),
      protoSymbol,
      layout,
    )

    val declarationName = symbol.displayName
    for {
      occurrence <- protoDocument.occurrences
      if declares(occurrence.symbol, declarationName)
    } result(occurrence.symbol) = protoSymbol
  }

  /**
   * Whether `symbol`'s owner chain passes through a level named `name`.
   *
   * Compared element by element rather than as a substring, so a message named
   * `User` is not matched by a `UserGroup` at the same level, and a nested
   * `Container#Inner#` is reached by both `Container` and `Inner` -- which is
   * correct, since both own it.
   */
  private def declares(symbol: String, name: String): Boolean =
    ProtoLayout.nameChainAfterPackage(Symbol(symbol)).contains(name)

  /**
   * Maps a proto field symbol to its Java accessor method equivalents.
   *
   * For enum values (ALL_CAPS), maps to Java static field. For regular fields,
   * maps to getter/setter methods, on both the message and its builder, and in
   * the package-less shape javac reports for an unresolved class.
   */
  private def mapFieldSymbol(
      protoSymbol: String,
      symbol: Symbol,
      layout: ProtoLayout,
      protoDocument: s.TextDocument,
      result: mutable.Map[String, String],
      accessorMethods: mutable.ListBuffer[String],
  ): Unit = {
    val fieldName = symbol.displayName
    val javaOwnerSymbol = javaSymbolOf(symbol.owner.value, layout)
    val javaClassName = Symbol(javaOwnerSymbol).displayName
    // The owner without its package, which is all javac reports when it could
    // not resolve the generated class.
    val bareOwnerSymbol =
      Symbols.Global(Symbols.None, Descriptor.Type(javaClassName))

    if (ProtoNaming.isAllCaps(fieldName)) {
      // A Java enum constant is a term. The method shape covers the accessor the
      // proto indexer emits for it, and the type shape is what javac reports
      // before it has resolved the enum at all.
      for (
        descriptor <- Seq(
          Descriptor.Term(fieldName),
          Descriptor.Method(fieldName, "()"),
        )
      )
        addSymbolMapping(
          result,
          Symbols.Global(javaOwnerSymbol, descriptor),
          protoSymbol,
          layout,
        )
      for (
        descriptor <- Seq(
          Descriptor.Term(fieldName),
          Descriptor.Type(fieldName),
        )
      )
        addSymbolMapping(
          result,
          Symbols.Global(bareOwnerSymbol, descriptor),
          protoSymbol,
          layout,
        )
    } else {
      val javaMethodNames = ProtoNaming.javaAccessorNames(fieldName)
      accessorMethods ++= javaMethodNames

      // protoc declares the setters on a `Builder` nested in the message, and
      // Metals' own outline on a top-level `<Msg>Builder`, so both spellings are
      // emitted -- generated at one and searched for at the other, a setter call
      // site would not be found.
      def withBuilders(owner: String) =
        Seq(
          owner,
          Symbols.Global(owner, Descriptor.Type("Builder")),
          Symbols.Global(
            Symbol(owner).owner.value,
            Descriptor.Type(Symbol(owner).displayName + "Builder"),
          ),
        ).distinct

      for {
        methodName <- javaMethodNames
        owner <- withBuilders(javaOwnerSymbol)
      } addSymbolMapping(
        result,
        Symbols.Global(owner, Descriptor.Method(methodName, "()")),
        protoSymbol,
        layout,
      )

      // Without a package, which is all javac reports for a class it could not
      // resolve -- and then it cannot tell a method from a type either.
      for {
        methodName <- javaMethodNames
        owner <- withBuilders(bareOwnerSymbol)
        descriptor <- Seq(
          Descriptor.Method(methodName, "()"),
          Descriptor.Type(methodName),
        )
      } addSymbolMapping(
        result,
        Symbols.Global(owner, descriptor),
        protoSymbol,
        layout,
      )

      // Whatever the proto document itself records for this field beats any
      // candidate generated above, being what the indexer actually emitted.
      for {
        occurrence <- protoDocument.occurrences
        occurrenceSymbol = Symbol(occurrence.symbol)
        if declares(occurrence.symbol, javaClassName) &&
          javaMethodNames.contains(occurrenceSymbol.displayName)
      } result(occurrence.symbol) = protoSymbol
    }
  }

  /**
   * The same declaration re-rooted at the generated code's package: proto symbol
   * `com/example/User#` with `java_package` "com.example.jproto" becomes
   * `com/example/jproto/User#`.
   *
   * Rebuilt descriptor by descriptor, so it does not care what the proto package
   * was -- including the case where the proto declares none.
   */
  def convertProtoSymbolToJava(
      protoSymbol: String,
      javaPackage: String,
  ): String =
    ProtoLayout.reroot(
      Symbol(protoSymbol),
      ProtoLayout.packageSymbolOf(javaPackage),
    )

  private def javaSymbolOf(protoSymbol: String, layout: ProtoLayout): String =
    ProtoLayout.reroot(Symbol(protoSymbol), javaPackageSymbol(layout))

  private def javaPackageSymbol(layout: ProtoLayout): String =
    ProtoLayout.packageSymbolOf(layout.javaPackage)

  private def addSymbolMapping(
      result: mutable.Map[String, String],
      javaSymbol: String,
      protoSymbol: String,
      layout: ProtoLayout,
  ): Unit = {
    result(javaSymbol) = protoSymbol
    // In the default layout every declaration is nested in the outer class, so
    // the same member has a second, deeper symbol. The name comes from the
    // layout, so it carries protoc's collision rule with it.
    if (!layout.javaMultipleFiles && layout.outerClassName.nonEmpty) {
      ProtoLayout
        .nestedInType(Symbol(javaSymbol), layout.outerClassName)
        .foreach(variant => result(variant) = protoSymbol)
    }
  }
}
