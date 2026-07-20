package scala.meta.internal.metals.decompile

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

/**
 * Materializes CFR's decompiled Java source for a `.class` file into a
 * read-only `.java` file on disk, so that goto-definition from *inside*
 * decompiled code works: a `.class` (or in-memory) URI reaches no
 * presentation compiler, but a real file under `.metals/readonly/` is picked
 * up like any other dependency source. Mirrors
 * [[scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles]].
 *
 * The originating jar's filename is folded into the path (see [[materialize]])
 * so that later navigations -- once only the materialized file remains, and
 * the original `.class` location is gone -- can still recover which jar it
 * came from (see [[jarFileNameOf]]) and route to a presentation compiler
 * backed by a build target with that jar on its classpath, the same way
 * [[scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles]] routes to the
 * build target owning a `.proto` file.
 */
object DecompiledJavaFiles {

  private val rootDirName = "decompiled"
  private val classDirectoryMarker = "workspace-classes"

  /**
   * Writes `content` -- CFR's decompilation of the class denoted by
   * `pathClass` -- to a stable on-disk location keyed by the originating
   * jar's filename (or [[classDirectoryMarker]] for a class-directory entry)
   * plus the class's own package and JVM binary name, and returns it. Keying
   * by the decompiled class's own name, rather than the symbol that
   * triggered navigation, keeps the file matching what was actually
   * decompiled.
   *
   * `None` when `pathClass` isn't inside a recognized classpath entry (a
   * jar, or one of `classDirectories`); callers should then fall back to the
   * unmaterialized `.class` location.
   */
  def materialize(
      workspace: AbsolutePath,
      pathClass: AbsolutePath,
      classDirectories: Seq[AbsolutePath],
      content: String,
  ): Option[AbsolutePath] =
    origin(pathClass, classDirectories).map { case (marker, segments) =>
      val pkg = segments.init
      val javaFile = (marker +: pkg)
        .foldLeft(root(workspace))(_.resolve(_))
        .resolve(s"${segments.last}.java")
      writeIfChanged(javaFile, content)
      javaFile
    }

  /**
   * The filename of the jar a materialized file (see [[materialize]]) was
   * decompiled from, recovered from its on-disk path. `None` if `path` isn't
   * a materialized decompiled file, or came from a class directory rather
   * than a jar.
   */
  def jarFileNameOf(
      workspace: AbsolutePath,
      path: AbsolutePath,
  ): Option[String] =
    path.toRelativeInside(root(workspace)).flatMap { rel =>
      rel.toNIO
        .iterator()
        .asScala
        .map(_.toString)
        .toSeq
        .headOption
        .filterNot(_ == classDirectoryMarker)
    }

  /**
   * `Outer$Inner` -> `Outer`; a top-level class path is returned unchanged.
   * A top-level Scala object's module class (`Foo$`) has a trailing `$`
   * too, but that's a module-class suffix, not a nesting separator, so it's
   * stripped first to avoid being mistaken for one.
   *
   * Used by [[scala.meta.internal.metals.Compilers.decompileAndLocate]] to
   * avoid decompiling a nested class in isolation: CFR can't see the
   * enclosing context then, and emits the invalid `class Outer.Inner`.
   * Decompiling the enclosing class instead gives valid, nested Java.
   */
  def topLevelClassPath(pathClass: AbsolutePath): AbsolutePath = {
    val withoutModuleSuffix = pathClass.filename.stripSuffix("$")
    val dollar = withoutModuleSuffix.indexOf('$')
    if (dollar < 0) {
      pathClass
    } else {
      pathClass.parent.resolve(withoutModuleSuffix.take(dollar))
    }
  }

  private def root(workspace: AbsolutePath): AbsolutePath =
    workspace.resolve(Directories.dependencies).resolve(rootDirName)

  /**
   * The marker directory segment (an originating jar's filename, or
   * [[classDirectoryMarker]]) plus the package and JVM binary name of the
   * class `pathClass` denotes (e.g. `List("com", "example", "Outer$Inner")`),
   * read from its path rather than parsed from a symbol string. A jar
   * entry's path is already rooted at the jar, i.e. already the package
   * path; a class-directory entry is made relative to whichever of
   * `classDirectories` contains it. `None` if neither applies.
   */
  private def origin(
      pathClass: AbsolutePath,
      classDirectories: Seq[AbsolutePath],
  ): Option[(String, Seq[String])] = {
    def segmentsOf(path: Path): Seq[String] =
      path.iterator().asScala.map(_.toString).toSeq

    if (pathClass.isJarFileSystem)
      pathClass.jarPath.map(jar => jar.filename -> segmentsOf(pathClass.toNIO))
    else
      classDirectories.iterator
        .flatMap(pathClass.toRelativeInside(_))
        .nextOption()
        .map(rel => classDirectoryMarker -> segmentsOf(rel.toNIO))
  }

  private def writeIfChanged(file: AbsolutePath, content: String): Unit = {
    val current =
      if (file.exists) Some(FileIO.slurp(file, StandardCharsets.UTF_8))
      else None
    if (!current.contains(content)) {
      Files.createDirectories(file.toNIO.getParent)
      Files.write(file.toNIO, content.getBytes(StandardCharsets.UTF_8))
    }
  }
}
