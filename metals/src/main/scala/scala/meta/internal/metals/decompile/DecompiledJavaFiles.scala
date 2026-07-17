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
 * read-only file on disk, mirroring
 * [[scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles]]: a `.class` (or
 * in-memory) URI reaches no presentation compiler at all, so goto-definition
 * from *inside* decompiled code was previously a dead end. A real `.java`
 * file under `.metals/readonly/` is picked up by a presentation compiler like
 * any other dependency source, so navigation from there works.
 *
 * The originating jar's filename is folded into the path (see [[materialize]])
 * so that a caller holding only the materialized file -- as happens on every
 * navigation *after* the first, once the `.class` location is gone -- can
 * still recover which jar it came from (see [[jarFileNameOf]]) and route to a
 * presentation compiler backed by a build target that actually has that jar
 * on its classpath, the same way [[scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles]]
 * routes to the build target owning a `.proto` file.
 */
object DecompiledJavaFiles {

  private val rootDirName = "decompiled"
  private val classDirectoryMarker = "workspace-classes"

  /**
   * Writes `content` -- CFR's decompilation of the class denoted by
   * `pathClass` -- to a stable on-disk location keyed by the originating
   * jar's filename (or [[classDirectoryMarker]] for a class-directory entry)
   * followed by the class's own package and JVM binary name, and returns it.
   * Keying by the decompiled class's own name (rather than the symbol that
   * triggered navigation) means the file always matches what was actually
   * decompiled, even when that's a nested class decompiled in isolation
   * (`Outer$Inner`, not `Outer`).
   *
   * `None` when `pathClass` isn't inside a recognized classpath entry (a
   * jar, or one of `classDirectories`), in which case the caller should keep
   * using the unmaterialized `.class` location.
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
   * decompiled from, recovered from its own on-disk path -- `None` when
   * `path` isn't a materialized decompiled file, or was decompiled from a
   * class directory rather than a jar.
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

  private def root(workspace: AbsolutePath): AbsolutePath =
    workspace.resolve(Directories.dependencies).resolve(rootDirName)

  /**
   * The marker directory segment (an originating jar's filename, or
   * [[classDirectoryMarker]]) followed by the package and JVM binary name of
   * the class `pathClass` denotes (e.g. `List("com", "example",
   * "Outer$Inner")`), read from its own path rather than parsed from a symbol
   * string. A jar entry's path is already rooted at the jar (i.e. it already
   * is the package path); a class-directory entry is made relative to
   * whichever of `classDirectories` contains it. `None` when neither applies.
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
