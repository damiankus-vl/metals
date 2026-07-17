package scala.meta.internal.metals.decompile

import java.net.URI

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.jpc.JavaMetalsCompiler
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.IdentifierTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.ParameterizedTypeTree
import com.sun.source.tree.Tree
import com.sun.source.util.TreeScanner
import org.eclipse.{lsp4j => l}

/**
 * How a symbol reaching the decompiled-classpath fallback should be navigated.
 * The presentation compiler encodes an unresolved inherited member as
 * `Owner#member#`, which by descriptor is indistinguishable from a nested type
 * `Owner#Nested#`. Rather than probe the classpath for a class named after the
 * member (which fabricates a nonsense `Owner$member.class` path), we classify
 * from the *owner*: a member or nested type is owned by a type, whereas a
 * top-level type is owned by its package. A member symbol therefore never
 * reaches a class-file lookup as if it were a class.
 */
private sealed trait SymbolKind
private object SymbolKind {

  /** The symbol is itself a class on the classpath. */
  final case class Type(typeSymbol: String) extends SymbolKind

  /** A member `memberName` reachable through `ownerSymbol`'s hierarchy. */
  final case class Member(ownerSymbol: String, memberName: String)
      extends SymbolKind
}

/**
 * Turns a JVM library symbol that the presentation compiler resolved without a
 * source location (common in MBT/Bazel workspaces) into `.class` definition
 * targets, walking the compiled type hierarchy so an inherited member reaches
 * its declaring class. Nothing is decompiled here; opening a returned `.class`
 * triggers Metals' existing consent-gated decompilation.
 *
 * @param classpathEntries
 *   jars and class directories to search, re-read on every call since the
 *   workspace classpath can change.
 * @param protoJavaOutlineFor
 *   the synthesized proto outline declaring a class; its supertypes become
 *   the walk's entry points when the owner isn't on the classpath.
 * @param classSourceFile
 *   the workspace source file declaring a class, if it has one. When a member's
 *   declaring class is workspace source, the target points at that source at
 *   the member's bytecode line (so a compiled-only member like a Lombok
 *   accessor lands on the annotated field) instead of at the `.class`.
 */
final class NavigationTargetProvider(
    classpathEntries: () => Iterator[AbsolutePath],
    protoJavaOutlineFor: String => Option[VirtualTextDocument],
    classSourceFile: String => Option[AbsolutePath],
)(implicit ec: ExecutionContext) {

  private val classFileHierarchyIndex =
    new ClassFileHierarchyIndex(classpathEntries)

  /**
   * Definition targets for `symbol`, each pairing a `.class` location with the
   * symbol rebased onto that class:
   *
   *  - a type symbol yields the single `.class` that declares it;
   *  - a member symbol yields every class in the owner's hierarchy that declares
   *    a member of the same name, so an inherited or overridden member offers
   *    each declaration (ancestors may live in different jars).
   */
  def classHierarchyTargets(
      rawSymbol: String
  ): Future[Seq[(String, l.Location)]] = Future {
    val symbol = recoverNestedClassSymbol(rawSymbol)
    val result = classify(symbol) match {
      case Some(SymbolKind.Type(classSymbol)) =>
        typeTargets(classSymbol)
      case Some(SymbolKind.Member(ownerSymbol, memberName)) =>
        val members = classFileHierarchyIndex
          .navigationTargets(entryPointClasses(ownerSymbol), memberName)
          .map(target =>
            target.memberSymbol -> preferSource(target, memberName)
          )
        // A type-owned symbol is usually a member, but the same shape also
        // describes a nested type the PC reported as `Owner#Nested#`. Only when
        // no class in the hierarchy declares the member do we fall back to
        // resolving it as that nested type.
        if (members.nonEmpty) members
        else typeTargets(symbol)
      case None => Nil
    }
    result
  }

  /**
   * Recovers the package/nested-class split for a symbol whose segments are
   * all `/`-joined, as if every enclosing class were a package -- either a
   * nested type reported as `Owner/Nested#` (instead of `Owner#Nested#`), or
   * the enclosing class itself misreported as a plain package (`Owner/`
   * instead of `Owner#`). This shape doesn't come from this class -- it's
   * what the presentation compiler itself reports when the classfile it read
   * for a dependency type doesn't carry (or wasn't given) `InnerClasses`
   * linkage back to its enclosing class, which happens for some dependency
   * classes in Bazel/MBT workspaces (Turbine's header classpath). Rather than
   * trust the reported split, probe the classpath at each growing prefix to
   * find where a real top-level `.class` exists, and re-`#`-join everything
   * from there on.
   *
   * A no-op when `symbol` already contains an internal `#` (assumed already
   * correctly split), doesn't end in `#` or `/`, or doesn't resolve to any
   * class file, so this is safe to apply unconditionally to every incoming
   * symbol.
   */
  private def recoverNestedClassSymbol(symbol: String): String = {
    if (symbol.endsWith("#") && !symbol.stripSuffix("#").contains("#"))
      reconstructNestedClassPath(symbol.stripSuffix("#"), symbol)
    else if (symbol.endsWith("/"))
      reconstructNestedClassPath(symbol.stripSuffix("/"), symbol)
    else symbol
  }

  private def reconstructNestedClassPath(
      path: String,
      fallback: String,
  ): String = {
    val segments = path.split('/').filter(_.nonEmpty)
    // The smallest leading-segment count whose joined prefix is a real
    // top-level class file -- everything before it is the package,
    // everything from it on (join by `#`) is the nested-class chain.
    val classStart =
      (1 to segments.length).find { count =>
        classFileHierarchyIndex
          .readClassFile(segments.take(count).mkString("/") + "#")
          .isDefined
      }
    classStart match {
      case None => fallback
      case Some(count) =>
        val pkg = segments.take(count - 1).mkString("/")
        val classes = segments.drop(count - 1).mkString("#") + "#"
        if (pkg.isEmpty) classes else s"$pkg/$classes"
    }
  }

  /**
   * When the member's declaring class is workspace source, a location in that
   * source at the member's bytecode line; otherwise the compiled `.class`
   * location (to be decompiled by the caller).
   */
  private def preferSource(
      target: NavigationTarget,
      memberName: String,
  ): l.Location = {
    val declaringClass = Symbol(target.memberSymbol).owner.value
    val sourceLocation =
      for {
        source <- classSourceFile(declaringClass)
        line <- classFileHierarchyIndex.memberSourceLine(
          declaringClass,
          memberName,
          target.methodDescriptor,
        )
      } yield new l.Location(
        source.toURI.toString,
        memberRange(source, line, memberName),
      )
    sourceLocation.getOrElse(target.classLocation)
  }

  /**
   * A range over the member's identifier on `oneBasedLine`, so navigation lands
   * on the name (matching a full Java language server, which lets the editor
   * collapse the two into one result) rather than selecting the whole line. A
   * compiled-only accessor (e.g. a Lombok `@Getter`) has no body, so its
   * bytecode line points at the annotated field; the field name derived from
   * the accessor is therefore tried too. Falls back to the line start when no
   * identifier is found.
   */
  private def memberRange(
      source: AbsolutePath,
      oneBasedLine: Int,
      memberName: String,
  ): l.Range = {
    val lineIndex = math.max(0, oneBasedLine - 1)
    val lineText = sourceLine(source, lineIndex)
    val candidates = memberName +: accessorFieldNames(memberName)
    val identifier =
      candidates
        .flatMap(name => identifierColumn(lineText, name).map(name -> _))
        .headOption
    identifier match {
      case Some((name, column)) =>
        new l.Range(
          new l.Position(lineIndex, column),
          new l.Position(lineIndex, column + name.length),
        )
      case None =>
        new l.Range(new l.Position(lineIndex, 0), new l.Position(lineIndex, 0))
    }
  }

  private def sourceLine(source: AbsolutePath, lineIndex: Int): String = {
    val lines = source.readTextOpt.map(_.split("\n", -1)).getOrElse(Array.empty)
    if (lineIndex >= 0 && lineIndex < lines.length) lines(lineIndex) else ""
  }

  /**
   * The column of `name` used as a whole identifier in `lineText` (not a
   * substring of a longer name), or `None` if it doesn't appear.
   */
  private def identifierColumn(lineText: String, name: String): Option[Int] = {
    def isIdentifierChar(c: Char): Boolean =
      c.isLetterOrDigit || c == '_' || c == '$'
    def search(from: Int): Option[Int] = {
      val index = lineText.indexOf(name, from)
      if (index < 0 || name.isEmpty) None
      else {
        val beforeOk =
          index == 0 || !isIdentifierChar(lineText.charAt(index - 1))
        val after = index + name.length
        val afterOk =
          after >= lineText.length || !isIdentifierChar(lineText.charAt(after))
        if (beforeOk && afterOk) Some(index)
        else search(index + 1)
      }
    }
    search(0)
  }

  /**
   * Candidate field names behind a `get`/`set`/`is` accessor, most likely
   * first: the JavaBean form (`getValue` -> `value`) and, for an all-caps field
   * whose leading letter isn't lowercased by convention, the raw form
   * (`getURL` -> `URL`).
   */
  private def accessorFieldNames(memberName: String): Seq[String] = {
    val stripped =
      if (memberName.startsWith("get") && memberName.length > 3)
        Some(memberName.substring(3))
      else if (memberName.startsWith("set") && memberName.length > 3)
        Some(memberName.substring(3))
      else if (memberName.startsWith("is") && memberName.length > 2)
        Some(memberName.substring(2))
      else None
    stripped.toSeq.flatMap { name =>
      val javaBean =
        if (name.isEmpty) name else name.head.toLower.toString + name.tail
      Seq(javaBean, name).distinct
    }
  }

  /**
   * Resolves against the enclosing top-level class's own `.class` file rather
   * than `classSymbol`'s (possibly nested) one, so a nested type is
   * decompiled as part of its whole enclosing class instead of in isolation.
   * CFR only renders valid, correctly-nested Java when a nested class is
   * decompiled together with its enclosing class; decompiled alone, it labels
   * the class `Outer.Inner` (see [[DecompiledDeclarationSearch]]). Returning
   * `classSymbol` unchanged alongside the enclosing class's location lets the
   * caller's occurrence search in the decompiled source land on the nested
   * class's own declaration line, matching how a nested proto-generated type
   * is already navigated (one materialized file per top-level outline,
   * landing on the nested message's line within it).
   */
  private def typeTargets(classSymbol: String): Seq[(String, l.Location)] = {
    val result =
      classFileHierarchyIndex
        .classFileLocation(topLevelClassOf(classSymbol))
        .map(classSymbol -> _)
        .toList
    result
  }

  /** The enclosing top-level class of a symbol, e.g. `Outer#Inner#` -> `Outer#`. */
  private def topLevelClassOf(classSymbol: String): String = {
    val firstHash = classSymbol.indexOf('#')
    if (firstHash < 0) classSymbol else classSymbol.substring(0, firstHash + 1)
  }

  /**
   * Classifies `symbol` from its shape alone — no class-file probe, so a member
   * symbol is never turned into a bogus `.class` path here. A symbol owned by a
   * type is a member (or a nested type reported the same way); a symbol owned by
   * a package is a top-level type.
   */
  private def classify(symbol: String): Option[SymbolKind] = {
    val sym = Symbol(symbol)
    val ownerSymbol = sym.owner.value
    val memberName = memberNameOf(symbol, ownerSymbol)
    val result =
      if (memberName.nonEmpty && Symbol(ownerSymbol).isType)
        Some(SymbolKind.Member(ownerSymbol, memberName))
      else if (sym.isType) Some(SymbolKind.Type(symbol))
      else None
    result
  }

  /** The member name from a resolved (`m().`, `m.`) or unresolved (`m#`) symbol. */
  private def memberNameOf(symbol: String, ownerSymbol: String): String = {
    val descriptor = symbol.stripPrefix(ownerSymbol)
    val paren = descriptor.indexOf('(')
    val result =
      if (paren >= 0) descriptor.substring(0, paren)
      else descriptor.stripSuffix("#").stripSuffix(".")
    result
  }

  /**
   * Where to start the hierarchy walk: the owner itself when it's a compiled
   * class on the classpath, otherwise the supertypes declared in its synthesized
   * proto outline. The presentation compiler is no help here: its `info` is
   * empty for Java files (the only files that reach this path) and it can't see
   * synthesized outlines anyway.
   */
  private def entryPointClasses(ownerSymbol: String): Seq[String] = {
    val result =
      if (classFileHierarchyIndex.readClassFile(ownerSymbol).isDefined) {
        Seq(ownerSymbol)
      } else {
        protoOutlineSupertypes(ownerSymbol).distinct
      }
    result
  }

  /** Supertype class symbols declared by the synthesized outline of `typeSymbol`. */
  private def protoOutlineSupertypes(classSymbol: String): Seq[String] = {
    val result =
      protoJavaOutlineFor(classSymbol).toSeq
        .flatMap(outline =>
          parseSupertypeSymbols(outline.text, simpleNameOf(classSymbol))
        )
    result
  }

  private def simpleNameOf(classSymbol: String): String = {
    val result = classSymbol.stripSuffix("#").split(Array('/', '#')).last
    result
  }

  private def parseSupertypeSymbols(
      text: String,
      simpleName: String,
  ): Seq[String] = {
    val result =
      classDeclaration(text, simpleName).toSeq.flatMap { classTree =>
        val extendsClause = Option(classTree.getExtendsClause()).toSeq
        // `.asScala` as an extension method resolves to `MetalsEnrichments`'s
        // `XtensionJavaList`, which is also applicable to any `util.List[A]`
        // but doesn't define `asScala` -- calling the stdlib converter
        // directly sidesteps that shadowing.
        val implementsClauses = scala.jdk.CollectionConverters
          .ListHasAsScala(
            classTree.getImplementsClause().asInstanceOf[java.util.List[Tree]]
          )
          .asScala
          .toSeq
        (extendsClause ++ implementsClauses).map(typeTreeToClassSymbol)
      }
    result
  }

  private val outlineUri = URI.create("file:///ProtoOutline.java")

  /**
   * The class/interface/enum declaration named `simpleName`, found anywhere in
   * `text` (top-level or nested), parsed with javac's own parser rather than
   * matched by regex. Unlike CFR's decompiled output (see
   * [[DecompiledDeclarationSearch]]), a synthesized proto outline is real,
   * compilable Java, so a proper parse handles generics, annotations, and
   * comments for free instead of needing bespoke text scanning for each.
   */
  private def classDeclaration(
      text: String,
      simpleName: String,
  ): Option[ClassTree] = {
    val result =
      for {
        (_, unit) <- JavaMetalsCompiler.parse(
          CompilerVirtualFileParams(outlineUri, text)
        )
        classTree <- findClassBySimpleName(unit, simpleName)
      } yield classTree
    result
  }

  /** Depth-first search over `unit`'s type declarations, including nested ones. */
  private def findClassBySimpleName(
      unit: CompilationUnitTree,
      simpleName: String,
  ): Option[ClassTree] = {
    val scanner = new TreeScanner[ClassTree, Unit] {
      override def visitClass(node: ClassTree, p: Unit): ClassTree = {
        val matchedHere =
          if (node.getSimpleName().contentEquals(simpleName)) node else null
        val matchedInMember = super.visitClass(node, p)
        if (matchedHere != null) matchedHere else matchedInMember
      }
      override def reduce(r1: ClassTree, r2: ClassTree): ClassTree =
        if (r1 != null) r1 else r2
    }
    Option(scanner.scan(unit, ()))
  }

  /** The class symbol of a type as written in an `extends`/`implements` clause. */
  private def typeTreeToClassSymbol(tree: Tree): String =
    fullyQualifiedNameToClassSymbol(qualifiedTypeName(tree))

  /** Strips generic type arguments structurally instead of via bracket-matching. */
  private def qualifiedTypeName(tree: Tree): String = tree match {
    case parameterized: ParameterizedTypeTree =>
      qualifiedTypeName(parameterized.getType())
    case memberSelect: MemberSelectTree =>
      s"${qualifiedTypeName(memberSelect.getExpression())}.${memberSelect.getIdentifier()}"
    case identifier: IdentifierTree =>
      identifier.getName().toString()
    case other => other.toString()
  }

  private def fullyQualifiedNameToClassSymbol(
      fullyQualifiedName: String
  ): String = {
    val result = fullyQualifiedName.replace('.', '/') + "#"
    result
  }
}
