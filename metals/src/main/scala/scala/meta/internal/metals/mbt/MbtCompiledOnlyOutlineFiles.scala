package scala.meta.internal.metals.mbt

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.MD5
import scala.meta.io.AbsolutePath

/**
 * Materializes the synthesized outline for a compiled-only class (see
 * [[MbtCompiledOnlyOutlineProvider]]) into a read-only file on disk, so that
 * goto-definition on e.g. an annotation-processor-generated class opens a
 * real file instead of an in-memory-only `file:` URI the editor can't read
 * (and, on "Create file", can't write either -- it isn't inside the
 * workspace at all). Mirrors [[ProtoGeneratedJavaFiles]] and
 * [[DecompiledJavaFiles]].
 *
 * Keyed by an MD5 of the owning build target's id rather than its raw URI,
 * since a BSP target URI isn't generally a valid path segment, and to keep
 * two different targets that happen to share a binary class name (e.g. both
 * declaring `example.Generated_Foo`) from colliding on disk.
 */
object MbtCompiledOnlyOutlineFiles {

  private val rootDirName = "compiled-only"

  /**
   * The stable on-disk location `binaryClassName` (owned by `buildTargetId`)
   * would be materialized to. Pure/no I/O, so it can be used as this file's
   * URI before the (lazily decompiled) content is known.
   */
  def pathFor(
      workspace: AbsolutePath,
      buildTargetId: String,
      binaryClassName: String,
  ): AbsolutePath = {
    val segments = binaryClassName.split('.').toIndexedSeq
    (MD5.compute(buildTargetId) +: segments.init)
      .foldLeft(root(workspace))(_.resolve(_))
      .resolve(s"${segments.last}.java")
  }

  /**
   * Writes `content` to [[pathFor]]'s location and returns it. Always
   * `Some` in practice (the path is always derivable), but `Option` for
   * consistency with its siblings [[ProtoGeneratedJavaFiles.materialize]] and
   * [[scala.meta.internal.metals.decompile.DecompiledJavaFiles.materialize]],
   * which can fail to resolve a path.
   */
  def materialize(
      workspace: AbsolutePath,
      buildTargetId: String,
      binaryClassName: String,
      content: String,
  ): Option[AbsolutePath] = {
    val javaFile = pathFor(workspace, buildTargetId, binaryClassName)
    writeIfChanged(javaFile, content)
    Some(javaFile)
  }

  /**
   * Partial inverse of [[pathFor]]: recovers the MD5 hash of the build
   * target id that owns the given materialized outline file, or `None` when
   * `path` isn't a materialized compiled-only outline. An MD5 hash can't be
   * un-hashed back into the original build target id, so callers must find a
   * build target whose id hashes to this value (see
   * `Compilers.compiledOnlyOutlineTarget`).
   */
  def originBuildTargetHash(
      workspace: AbsolutePath,
      path: AbsolutePath,
  ): Option[String] =
    path
      .toRelativeInside(root(workspace))
      .flatMap(_.toNIO.iterator().asScala.map(_.toString).nextOption())

  /**
   * Partial inverse of [[pathFor]]: recovers the binary class name of the
   * given materialized outline file, or `None` when `path` isn't a
   * materialized compiled-only outline. Inverse of the `segments` join in
   * [[pathFor]] -- drops the leading build-target-hash directory, then
   * rejoins the remaining package/class segments with `.` and strips the
   * `.java` suffix from the last one.
   */
  def originBinaryClassName(
      workspace: AbsolutePath,
      path: AbsolutePath,
  ): Option[String] =
    path.toRelativeInside(root(workspace)).flatMap { rel =>
      val segments = rel.toNIO.iterator().asScala.map(_.toString).toSeq
      if (segments.size < 2) None
      else {
        val classSegments = segments.tail
        val className = classSegments.last.stripSuffix(".java")
        Some((classSegments.init :+ className).mkString("."))
      }
    }

  private def root(workspace: AbsolutePath): AbsolutePath =
    workspace.resolve(Directories.dependencies).resolve(rootDirName)

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
