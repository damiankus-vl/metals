package scala.meta.internal.metals.decompile

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}

/**
 * Turns a JVM library symbol the presentation compiler resolved without a
 * source location (common in MBT/Bazel workspaces) into a `.class` definition
 * target. Nothing is decompiled here; opening the returned `.class` triggers
 * Metals' existing consent-gated decompilation.
 *
 * @param classpathEntries
 *   jars and class directories to search; re-read on every call since the
 *   workspace classpath can change.
 */
final class NavigationTargetProvider(
    classpathEntries: () => Iterator[AbsolutePath]
)(implicit ec: ExecutionContext) {

  private val classFileHierarchyIndex =
    new ClassFileHierarchyIndex(classpathEntries)

  /**
   * The `.class` definition target for `symbol`, if it names a compiled type
   * (top-level or nested).
   */
  def classHierarchyTargets(
      rawSymbol: String
  ): Future[Seq[(String, l.Location)]] = Future {
    val symbol = recoverNestedClassSymbol(rawSymbol)
    if (Symbol(symbol).isType) typeTargets(symbol) else Nil
  }

  /**
   * Recovers the package/nested-class split for a symbol whose segments are
   * all `/`-joined, as if every enclosing class were a package: either a
   * nested type reported as `Owner/Nested#` (instead of `Owner#Nested#`), or
   * the enclosing class itself misreported as a package (`Owner/` instead of
   * `Owner#`). This happens when the presentation compiler reads a dependency
   * classfile lacking `InnerClasses` linkage to its enclosing class -- notably
   * for Bazel/MBT dependencies built through Turbine's header classpath.
   * Rather than trust the reported split, probe the classpath at each growing
   * prefix for a real top-level `.class`, then re-`#`-join from there.
   *
   * A no-op when `symbol` already has an internal `#`, doesn't end in `#` or
   * `/`, or resolves to no class file -- safe to apply unconditionally.
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
    // Smallest segment count whose prefix is a real top-level class file:
    // everything before it is the package, everything from it on (joined by
    // `#`) is the nested-class chain.
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
   * Resolves the enclosing top-level class's `.class` file, not
   * `classSymbol`'s own (possibly nested) one -- CFR only renders valid,
   * correctly-nested Java when a nested class is decompiled together with its
   * enclosing class; decompiled alone it's labeled `Outer.Inner` (see
   * [[DecompiledDeclarationSearch]]). `classSymbol` is returned unchanged
   * alongside that location so the caller's occurrence search lands on the
   * nested class's own declaration line.
   */
  private def typeTargets(classSymbol: String): Seq[(String, l.Location)] =
    classFileHierarchyIndex
      .classFileLocation(topLevelClassOf(classSymbol))
      .map(classSymbol -> _)
      .toList

  /** The enclosing top-level class of a symbol, e.g. `Outer#Inner#` -> `Outer#`. */
  private def topLevelClassOf(classSymbol: String): String = {
    val firstHash = classSymbol.indexOf('#')
    if (firstHash < 0) classSymbol else classSymbol.substring(0, firstHash + 1)
  }
}
