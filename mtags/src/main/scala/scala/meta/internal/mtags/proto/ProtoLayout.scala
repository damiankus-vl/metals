package scala.meta.internal.mtags.proto

import scala.jdk.CollectionConverters._

import scala.meta.inputs.Input
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.proto.diag.SourceFile
import scala.meta.internal.proto.parse.Parser
import scala.meta.internal.proto.tree.Proto._
import scala.meta.internal.semanticdb.Scala._

/**
 * What a `.proto` file says about the code generated from it, and the two tests
 * that decide whether a generated symbol came from this file.
 *
 * Generators agree on very little. They agree that the generated type lives
 * under the package the proto configures, and that it keeps the name the proto
 * declares. Everything between those two -- an outer class, nothing at all, a
 * sub-package named after the file -- is what they disagree on, so it is
 * deliberately not part of either test.
 */
final class ProtoLayout private (
    val protoPath: String,
    val protoPackage: String,
    val javaPackage: String,
    val javaMultipleFiles: Boolean,
    val explicitOuterClassName: Option[String],
    val declarations: Seq[ProtoDeclaration],
    packagePrefix: String
) {

  /** `foo_bar`, for `a/b/foo_bar.proto`. */
  def baseName: String = ProtoNaming.protoBaseName(protoPath)

  def topLevelDeclarations: Seq[ProtoDeclaration] =
    declarations.filter(_.isTopLevel)

  lazy val declaredNames: Set[String] = declarations.map(_.name).toSet

  lazy val outerClassName: String =
    ProtoNaming.outerClassName(
      protoPath,
      explicitOuterClassName,
      topLevelDeclarations.map(_.name).toSet
    )

  /**
   * The packages a proto document is indexed under: the proto package, plus
   * `java_package` when it differs.
   *
   * Both are registered because a generated Java symbol resolves under the
   * second while proto-to-proto navigation uses the first. When no
   * `java_package` option is set the two coincide, and then a Java symbol
   * resolves straight to the `.proto` -- which is why consumers filter hits by
   * file extension rather than assuming an index hit is generated code.
   */
  lazy val semanticdbPackages: Seq[String] = {
    val protoPackages =
      if (protoPackage.isEmpty) Seq(Symbols.EmptyPackage)
      else Seq(ProtoLayout.packageSymbolOf(protoPackage))
    if (javaPackage.nonEmpty && javaPackage != protoPackage)
      protoPackages :+ ProtoLayout.packageSymbolOf(javaPackage)
    else protoPackages
  }

  /**
   * Every package symbol generated code from this proto could be rooted at:
   * [[semanticdbPackages]] plus the prefixed variant when the workspace shades
   * the protobuf runtime into another package.
   *
   * Kept separate from [[semanticdbPackages]] because that one is what the index
   * is keyed by and must not shift with a user setting.
   */
  lazy val configuredPackages: Seq[String] = {
    val prefixed =
      if (packagePrefix.isEmpty) Nil
      else
        for {
          declaredPackage <- Seq(javaPackage, protoPackage).filter(_.nonEmpty)
        } yield ProtoLayout.packageSymbolOf(packagePrefix + declaredPackage)
    (semanticdbPackages ++ prefixed).distinct
  }

  /**
   * Names that come from the proto's *file name* rather than any declaration in
   * it: the outer class, ScalaPB's file object, and ScalaPB's per-file
   * sub-package segment.
   *
   * These are what identify which proto in a package a symbol belongs to when
   * its owner chain never touches a declared type -- a bare `x/y/FooBar#` names
   * nothing the proto body mentions.
   */
  lazy val fileDerivedNames: Set[String] =
    Set(
      outerClassName,
      baseName,
      ProtoNaming.snakeToUpperCamel(baseName),
      ProtoNaming.snakeToUpperCamel(baseName) + "Proto"
    ).filter(_.nonEmpty)

  /**
   * The sub-packages of `packageSymbol` a generator could name after this file,
   * as ScalaPB does -- `com/example/api/` becomes `com/example/api/foo_bar/`.
   */
  def fileDerivedSubPackages(packageSymbol: String): Seq[String] =
    if (baseName.isEmpty) Nil
    else Seq(Symbols.Global(packageSymbol, Descriptor.Package(baseName)))

  /**
   * Whether `symbol` sits under one of [[configuredPackages]], allowing deeper
   * packages so that a generator nesting its output in a sub-package still
   * matches. The first of the two invariants.
   */
  def enclosesPackageOf(symbol: Symbol): Boolean =
    enclosingConfiguredPackage(symbol).isDefined

  /**
   * The configured package `symbol` is rooted at, and the package segments the
   * generator inserted below it -- `List("model")` for
   * `com/example/api/jproto/model/User#` against `com/example/api/jproto/`.
   */
  def enclosingConfiguredPackage(
      symbol: Symbol
  ): Option[(String, List[String])] = {
    def loop(
        candidate: Symbol,
        extraSegments: List[String]
    ): Option[(String, List[String])] = {
      if (candidate.isNone) None
      else if (configuredPackages.contains(candidate.value))
        Some((candidate.value, extraSegments))
      else if (candidate.isRootPackage || candidate.isEmptyPackage) None
      else loop(candidate.owner, candidate.displayName :: extraSegments)
    }
    loop(symbol.enclosingPackage, Nil)
  }

  /**
   * Whether this proto could have generated `symbol`, and on what evidence.
   *
   * The second invariant, checked against the owner chain rather than against a
   * fixed nesting: a declared name anywhere in the chain is what ties the symbol
   * to a declaration, and a file-derived name is what ties it to this file when
   * the chain names no declaration at all.
   */
  def matches(symbol: Symbol): Option[ProtoNameMatch] =
    for {
      (_, extraPackageSegments) <- enclosingConfiguredPackage(symbol)
      chain = ProtoLayout.nameChainAfterPackage(symbol)
      matched <- matchDeclaration(chain)
        .orElse(matchFileDerivedName(chain, extraPackageSegments))
    } yield matched

  /**
   * The declaration a chain element names, deepest first so that a nested
   * message wins over its parent, and so that a member's owner is preferred over
   * an enclosing outer class.
   */
  private def matchDeclaration(chain: List[String]): Option[ProtoNameMatch] =
    chain.reverse
      .flatMap(name =>
        ProtoNaming
          .declarationNameCandidates(name)
          .flatMap(candidate => declarations.filter(_.name == candidate))
      )
      .headOption
      .map(declaration =>
        ProtoNameMatch(ProtoNameEvidence.DeclaredType, Some(declaration))
      )

  private def matchFileDerivedName(
      chain: List[String],
      extraPackageSegments: List[String]
  ): Option[ProtoNameMatch] =
    if ((chain ++ extraPackageSegments).exists(fileDerivedNames.contains))
      Some(ProtoNameMatch(ProtoNameEvidence.FileDerivedName, None))
    else None

  /**
   * The top-level symbols a generated file named `className` declares, in the
   * order a compiler should see them.
   *
   * Under `java_multiple_files` a message's file holds the accessor interface
   * too -- `Msg.java` declares both `Msg` and `MsgOrBuilder` -- and the
   * accessors live on the interface, so a file recorded with one top-level
   * symbol hides half of what it defines. The name derived from the file itself
   * stays first: consumers read the head of the list as the file's binary name.
   */
  def topLevelSymbolsOf(
      packageSymbol: String,
      className: String
  ): Seq[String] = {
    val self = Symbols.Global(packageSymbol, Descriptor.Type(className))
    val orBuilder =
      if (
        javaMultipleFiles && declarations.exists(declaration =>
          declaration.isTopLevel && declaration.isMessage &&
            declaration.name == className
        )
      )
        Seq(
          Symbols
            .Global(packageSymbol, Descriptor.Type(className + "OrBuilder"))
        )
      else Nil
    self +: orBuilder
  }
}

object ProtoLayout {

  def fromInput(
      input: Input.VirtualFile,
      packagePrefix: String = ""
  ): ProtoLayout =
    fromProtoFile(
      Parser.parse(new SourceFile(input.path, input.text)),
      input.path,
      packagePrefix
    )

  def fromProtoFile(
      protoFile: ProtoFile,
      protoPath: String,
      packagePrefix: String = ""
  ): ProtoLayout = {
    val protoPackage =
      if (protoFile.pkg().isPresent) protoFile.pkg().get().fullName() else ""
    val javaPackage =
      stringOption(protoFile, "java_package").getOrElse(protoPackage)
    new ProtoLayout(
      protoPath = protoPath,
      protoPackage = protoPackage,
      javaPackage = javaPackage,
      javaMultipleFiles =
        stringOption(protoFile, "java_multiple_files").contains("true"),
      explicitOuterClassName = stringOption(protoFile, "java_outer_classname"),
      declarations = declarationsOf(protoFile, protoPackage),
      packagePrefix = normalizePrefix(packagePrefix)
    )
  }

  /**
   * The owner names between `symbol` and its enclosing package, outermost
   * first: `com/example/Foo#Bar#baz().` becomes `List("Foo", "Bar", "baz")`.
   */
  def nameChainAfterPackage(symbol: Symbol): List[String] = {
    val enclosingPackage = symbol.enclosingPackage
    def loop(current: Symbol, acc: List[String]): List[String] =
      if (
        current.isNone || current.isRootPackage || current.isEmptyPackage ||
        current == enclosingPackage
      ) acc
      else loop(current.owner, current.displayName :: acc)
    loop(symbol, Nil)
  }

  /**
   * The descriptors between `symbol` and its enclosing package, outermost first:
   * `com/example/Foo#bar().` becomes `List(Type(Foo), Method(bar))`.
   *
   * Unlike [[nameChainAfterPackage]] this keeps the kind of each level, which is
   * what a caller rebuilding a symbol needs -- `Foo#bar.` and `Foo#bar().` and
   * `Foo#bar#` are three different symbols, and a name chain cannot tell them
   * apart.
   */
  def descriptorsAfterPackage(symbol: Symbol): List[Descriptor] = {
    def loop(current: String, acc: List[Descriptor]): List[Descriptor] =
      if (current.isNone || current.isRootPackage || current.isEmptyPackage) acc
      else {
        val (descriptor, owner) = DescriptorParser(current)
        descriptor match {
          case _: Descriptor.Package => acc
          case Descriptor.None => acc
          case _ => loop(owner, descriptor :: acc)
        }
      }
    loop(symbol.value, Nil)
  }

  /**
   * `symbol` with its enclosing package replaced by `packageSymbol`, rebuilt
   * descriptor by descriptor: `a/b/Msg#name().` re-rooted at `x/y/` becomes
   * `x/y/Msg#name().`.
   *
   * Not a string substitution, so it also works for a symbol that carries no
   * package at all -- which is how javac reports a class it could not resolve.
   */
  def reroot(symbol: Symbol, packageSymbol: String): String =
    descriptorsAfterPackage(symbol).foldLeft(packageSymbol)(
      (owner, descriptor) => Symbols.Global(owner, descriptor)
    )

  /**
   * `symbol` with `typeName` inserted directly under its package, which is where
   * an outer class sits: `x/y/Msg#name().` becomes `x/y/Outer#Msg#name().`.
   *
   * `None` when the symbol already starts with that type, so a caller cannot nest
   * the outer class inside itself.
   */
  def nestedInType(symbol: Symbol, typeName: String): Option[String] = {
    val descriptors = descriptorsAfterPackage(symbol)
    if (descriptors.headOption.contains(Descriptor.Type(typeName))) None
    else
      Some(
        descriptors.foldLeft(
          Symbols
            .Global(symbol.enclosingPackage.value, Descriptor.Type(typeName))
        )((owner, descriptor) => Symbols.Global(owner, descriptor))
      )
  }

  /**
   * Whether two owner chains name the same declaration, given that each side
   * nests levels the other knows nothing about: the generated side adds an outer
   * class and `Builder`, the proto side adds an enclosing `oneof`, and a
   * generator rooting its output in a sub-package moves a level into the package.
   * So one chain ending with the other is as much agreement as there is to be
   * had -- `List("Model", "User")` corresponds to `List("User")`.
   */
  def nameChainsCorrespond(lhs: List[String], rhs: List[String]): Boolean =
    lhs == rhs || endsWith(lhs, rhs) || endsWith(rhs, lhs)

  /**
   * Whether `outer` is the chain of the type `inner` lives in, which is the
   * outer-class layout: `List("Outer")` is where `List("Outer", "Msg")` is
   * declared, and a nested message stays inside its parent's file even under
   * `java_multiple_files`.
   */
  def nameChainEncloses(outer: List[String], inner: List[String]): Boolean =
    outer.nonEmpty && inner.startsWith(outer)

  private def endsWith(full: List[String], suffix: List[String]): Boolean =
    suffix.nonEmpty && full.lengthCompare(suffix.length) >= 0 &&
      full.takeRight(suffix.length) == suffix

  private def normalizePrefix(prefix: String): String =
    if (prefix.isEmpty || prefix.endsWith(".")) prefix else prefix + "."

  /**
   * The SemanticDB package symbol for a dotted package name, built descriptor by
   * descriptor rather than by replacing `.` with `/`, which loses the
   * root-package case and cannot express the empty package.
   */
  def packageSymbolOf(dottedPackage: String): String =
    dottedPackage
      .split('.')
      .iterator
      .filter(_.nonEmpty)
      .foldLeft(Symbols.RootPackage) { (owner, segment) =>
        Symbols.Global(owner, Descriptor.Package(segment))
      }

  private def stringOption(
      protoFile: ProtoFile,
      optionName: String
  ): Option[String] =
    protoFile
      .options()
      .asScala
      .find(option =>
        option.name().asScala.map(_.value()).mkString(".") == optionName
      )
      .flatMap(option =>
        option.value() match {
          case ident: Ident =>
            val value = ident.value()
            if (
              value.startsWith("\"") && value.endsWith("\"") &&
              value.length >= 2
            )
              Some(value.substring(1, value.length - 1))
            else Some(value)
          case _ => None
        }
      )

  private def declarationsOf(
      protoFile: ProtoFile,
      protoPackage: String
  ): Seq[ProtoDeclaration] = {
    // Root, not the empty package: the proto indexer roots its declarations at
    // `Symbols.RootPackage` when the file declares no package, so a symbol from
    // a package-less proto is `Msg#` and not `_empty_/Msg#`.
    val packageSymbol = packageSymbolOf(protoPackage)

    def message(
        declaration: MessageDecl,
        owner: String,
        isTopLevel: Boolean
    ): Seq[ProtoDeclaration] = {
      val name = declaration.name().value()
      val symbol = Symbols.Global(owner, Descriptor.Type(name))
      val nested = for {
        nestedMessage <- declaration.nestedMessages().asScala.toSeq
        nestedDeclaration <- message(nestedMessage, symbol, isTopLevel = false)
      } yield nestedDeclaration
      val nestedEnums = for {
        nestedEnum <- declaration.nestedEnums().asScala.toSeq
      } yield ProtoDeclaration(
        nestedEnum.name().value(),
        Symbols.Global(symbol, Descriptor.Type(nestedEnum.name().value())),
        ProtoDeclarationKind.Enum,
        isTopLevel = false
      )
      ProtoDeclaration(
        name,
        symbol,
        ProtoDeclarationKind.Message,
        isTopLevel
      ) +: (nested ++ nestedEnums)
    }

    protoFile.declarations().asScala.toSeq.flatMap {
      case declaration: MessageDecl =>
        message(declaration, packageSymbol, isTopLevel = true)
      case declaration: EnumDecl =>
        val name = declaration.name().value()
        Seq(
          ProtoDeclaration(
            name,
            Symbols.Global(packageSymbol, Descriptor.Type(name)),
            ProtoDeclarationKind.Enum,
            isTopLevel = true
          )
        )
      case declaration: ServiceDecl =>
        val name = declaration.name().value()
        Seq(
          ProtoDeclaration(
            name,
            Symbols.Global(packageSymbol, Descriptor.Type(name)),
            ProtoDeclarationKind.Service,
            isTopLevel = true
          )
        )
      case _ => Nil
    }
  }
}

/** A message, enum, or service the proto declares. */
final case class ProtoDeclaration(
    name: String,
    symbol: String,
    kind: ProtoDeclarationKind,
    isTopLevel: Boolean
) {
  def isMessage: Boolean = kind == ProtoDeclarationKind.Message
  def isService: Boolean = kind == ProtoDeclarationKind.Service
}

sealed trait ProtoDeclarationKind
object ProtoDeclarationKind {
  case object Message extends ProtoDeclarationKind
  case object Enum extends ProtoDeclarationKind
  case object Service extends ProtoDeclarationKind
}

/** Why a symbol is attributed to a proto, strongest first. */
sealed abstract class ProtoNameEvidence(val rank: Int)
object ProtoNameEvidence {

  /** The chain names a type the proto declares. */
  case object DeclaredType extends ProtoNameEvidence(0)

  /**
   * The chain names nothing declared, but does name something derived from the
   * proto's file name -- the outer class, a file object, a sub-package.
   */
  case object FileDerivedName extends ProtoNameEvidence(1)
}

final case class ProtoNameMatch(
    evidence: ProtoNameEvidence,
    declaration: Option[ProtoDeclaration]
)
