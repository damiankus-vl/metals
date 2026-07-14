package scala.meta.internal.metals.decompile

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}

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
    val sym = Symbol(symbol)
    // Direct type navigation: the symbol itself is a class on the classpath.
    if (sym.isType && classfileHierarchyIndex.readClassFile(symbol).isDefined)
      classfileHierarchyIndex.classFileLocation(symbol).map(symbol -> _).toList
    else {
      val ownerSymbol = sym.owner.value
      val memberName = memberNameOf(symbol, ownerSymbol)
      if (memberName.isEmpty || !Symbol(ownerSymbol).isType) Nil
      else
        classfileHierarchyIndex
          .hierarchyMemberTargets(seedClasses(ownerSymbol), memberName)
    }
  }

  /** The member name from a resolved (`m().`, `m.`) or unresolved (`m#`) symbol. */
  private def memberNameOf(symbol: String, ownerSymbol: String): String = {
    val descriptor = symbol.stripPrefix(ownerSymbol)
    val paren = descriptor.indexOf('(')
    if (paren >= 0) descriptor.substring(0, paren)
    else descriptor.stripSuffix("#").stripSuffix(".")
  }

  /**
   * Where to start the hierarchy walk: the owner itself when it's a compiled
   * class on the classpath, otherwise the supertypes declared in its synthesized
   * proto outline. The presentation compiler is no help here: its `info` is
   * empty for Java files (the only files that reach this path) and it can't see
   * synthesized outlines anyway.
   */
  private def seedClasses(ownerSymbol: String): Seq[String] =
    if (classfileHierarchyIndex.readClassFile(ownerSymbol).isDefined)
      Seq(ownerSymbol)
    else protoOutlineSupertypes(ownerSymbol).distinct

  /** Supertype class symbols declared by the synthesized outline of `classSymbol`. */
  private def protoOutlineSupertypes(classSymbol: String): Seq[String] =
    protoJavaOutlineFor(classSymbol).toSeq
      .flatMap(outline =>
        parseSupertypeSymbols(outline.text, simpleNameOf(classSymbol))
      )

  private def simpleNameOf(classSymbol: String): String =
    classSymbol.stripSuffix("#").split(Array('/', '#')).last

  private def parseSupertypeSymbols(
      text: String,
      simpleName: String,
  ): Seq[String] = {
    val start = text.indexOf(s"class $simpleName ")
    if (start < 0) Nil
    else {
      val brace = text.indexOf('{', start)
      val header = text.substring(start, if (brace < 0) text.length else brace)
      def listAfter(keyword: String): Seq[String] = {
        val i = header.indexOf(s" $keyword ")
        if (i < 0) Nil
        else {
          val rest = header.substring(i + keyword.length + 2)
          val stops =
            Seq(rest.indexOf(" extends "), rest.indexOf(" implements "))
              .filter(_ >= 0)
          val end = if (stops.isEmpty) rest.length else stops.min
          rest.substring(0, end).split(',').map(_.trim).filter(_.nonEmpty).toSeq
        }
      }
      (listAfter("extends") ++ listAfter("implements")).map(fqnToClassSymbol)
    }
  }

  private def fqnToClassSymbol(fqn: String): String =
    fqn.takeWhile(_ != '<').trim.replace('.', '/') + "#"
}
