package scala.meta.internal.metals.decompile

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

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
private sealed trait SymbolNavigation
private object SymbolNavigation {

  /** The symbol is itself a class on the classpath. */
  final case class AsType(classSymbol: String) extends SymbolNavigation

  /** A member `memberName` reachable through `ownerSymbol`'s hierarchy. */
  final case class AsMember(ownerSymbol: String, memberName: String)
      extends SymbolNavigation
}

/**
 * Turns a JVM library symbol that the presentation compiler resolved without a
 * source location (common in MBT/Bazel workspaces) into `.class` definition
 * targets, walking the compiled type hierarchy so an inherited member reaches
 * its declaring class. Nothing is decompiled here; opening a returned `.class`
 * triggers Metals' existing consent-gated decompilation.
 *
 * @param classpathJars
 *   jars to search, re-read on every call since the classpath can change.
 * @param protoJavaOutlineFor
 *   the synthesized proto outline declaring a class; its supertypes seed the
 *   walk when the owner isn't on the classpath.
 */
final class ClassHierarchyTargetProvider(
    classpathJars: () => Iterator[AbsolutePath],
    protoJavaOutlineFor: String => Option[VirtualTextDocument],
)(implicit ec: ExecutionContext) {

  private val classfileHierarchyIndex =
    new ClassfileHierarchyIndex(classpathJars)

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
      symbol: String
  ): Future[Seq[(String, l.Location)]] = Future {
    val result = classify(symbol) match {
      case Some(SymbolNavigation.AsType(classSymbol)) =>
        typeTargets(classSymbol)
      case Some(SymbolNavigation.AsMember(ownerSymbol, memberName)) =>
        val members = classfileHierarchyIndex
          .hierarchyMemberTargets(seedClasses(ownerSymbol), memberName)
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

  private def typeTargets(classSymbol: String): Seq[(String, l.Location)] = {
    val result =
      classfileHierarchyIndex
        .classFileLocation(classSymbol)
        .map(classSymbol -> _)
        .toList
    result
  }

  /**
   * Classifies `symbol` from its shape alone — no class-file probe, so a member
   * symbol is never turned into a bogus `.class` path here. A symbol owned by a
   * type is a member (or a nested type reported the same way); a symbol owned by
   * a package is a top-level type.
   */
  private def classify(symbol: String): Option[SymbolNavigation] = {
    val sym = Symbol(symbol)
    val ownerSymbol = sym.owner.value
    val memberName = memberNameOf(symbol, ownerSymbol)
    val result =
      if (memberName.nonEmpty && Symbol(ownerSymbol).isType)
        Some(SymbolNavigation.AsMember(ownerSymbol, memberName))
      else if (sym.isType) Some(SymbolNavigation.AsType(symbol))
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
  private def seedClasses(ownerSymbol: String): Seq[String] = {
    val result =
      if (classfileHierarchyIndex.readClassFile(ownerSymbol).isDefined)
        Seq(ownerSymbol)
      else protoOutlineSupertypes(ownerSymbol).distinct
    result
  }

  /** Supertype class symbols declared by the synthesized outline of `classSymbol`. */
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
    val start = text.indexOf(s"class $simpleName ")
    val result =
      if (start < 0) Nil
      else {
        val brace = text.indexOf('{', start)
        val header =
          text.substring(start, if (brace < 0) text.length else brace)
        def listAfter(keyword: String): Seq[String] = {
          val i = header.indexOf(s" $keyword ")
          if (i < 0) Nil
          else {
            val rest = header.substring(i + keyword.length + 2)
            val stops =
              Seq(rest.indexOf(" extends "), rest.indexOf(" implements "))
                .filter(_ >= 0)
            val end = if (stops.isEmpty) rest.length else stops.min
            rest
              .substring(0, end)
              .split(',')
              .map(_.trim)
              .filter(_.nonEmpty)
              .toSeq
          }
        }
        (listAfter("extends") ++ listAfter("implements")).map(fqnToClassSymbol)
      }
    result
  }

  private def fqnToClassSymbol(fqn: String): String = {
    val result = fqn.takeWhile(_ != '<').trim.replace('.', '/') + "#"
    result
  }
}
