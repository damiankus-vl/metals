package scala.meta.internal.metals

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

/**
 * Materializes the Java source obtained by decompiling a `.class` file from a
 * dependency jar into a read-only file on disk, so that goto-definition on a
 * JVM library class with no indexed source can open it and navigation can
 * continue from inside the decompiled source.
 *
 * Mirrors [[scala.meta.internal.metals.mbt.ProtoGeneratedJavaFiles]]: files
 * live under `.metals/readonly/` so clients treat them as read-only dependency
 * sources, and writes are idempotent.
 */
object DecompiledJavaFiles {

  /** Subdirectory of `.metals/readonly/dependencies` for decompiled sources. */
  private val rootDirName = "decompiled"

  /**
   * Writes the decompiled `content` for `javaPackagePath/className.java`
   * (originating from the jar named `jarFileName`) to a stable on-disk location
   * and returns it.
   *
   * @param javaPackagePath slash-separated package with a trailing slash
   *                        (e.g. `com/google/protobuf/`), or empty for the
   *                        default package
   */
  def materialize(
      workspace: AbsolutePath,
      jarFileName: String,
      javaPackagePath: String,
      className: String,
      content: String,
  ): Option[AbsolutePath] =
    try {
      val javaFile = workspace
        .resolve(Directories.dependencies)
        .resolve(rootDirName)
        .resolve(jarFileName)
        .resolveZipPath(Paths.get(javaPackagePath))
        .resolve(s"$className.java")
      writeIfChanged(javaFile, content)
      Some(javaFile)
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"decompiled-java: failed to materialize decompiled source for $className",
          e,
        )
        None
    }

  /**
   * Whether `path` is a Java source materialized by [[materialize]], i.e. it
   * lives under the `decompiled` root.
   */
  def isDecompiledJavaFile(
      workspace: AbsolutePath,
      path: AbsolutePath,
  ): Boolean = {
    val root = workspace
      .resolve(Directories.dependencies)
      .resolve(rootDirName)
    path.isJavaFilename && path.toRelativeInside(root).isDefined
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
