package scala.meta.internal.metals.mbt

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime

import scala.util.control.NonFatal

import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.MD5
import scala.meta.io.AbsolutePath

/**
 * A synthesized outline as a source file.
 *
 * `packageSymbol` is the package the outline declares, `com/example/jproto/`.
 * `file` is where the outline would be written,
 * `.metals/readonly/dependencies/proto-generated/a/model.proto/User.java`. The
 * path does not encode the package. It names the outline even when no file has
 * been written.
 */
final case class ProtoOutlineFile(
    packageSymbol: String,
    file: Path,
    text: String,
)

/**
 * Materializes the Java source that Metals synthesizes from a `.proto` file
 * (via [[MbtProtobufWorkspaceSymbolProvider]]) into a read-only file on disk.
 *
 * Two callers need a real file. Goto-definition needs one for the editor to
 * open. The Scala 3 compiler needs one because it cannot read the text from
 * memory, unlike Scala 2. A file on disk does not mean the user navigated to
 * it.
 *
 * The outline comes from the `.proto` alone. This needs no build output and no
 * configuration, and works for any build tool. Files live under
 * `.metals/readonly/` so clients treat them as read-only dependency sources.
 * They are dated 1970 so a compiled class of the same name still wins.
 */
object ProtoGeneratedJavaFiles {

  /** Subdirectory of `.metals/readonly/dependencies` for generated outlines. */
  private val rootDirName = "proto-generated"

  /**
   * Writes `content` for `className.java` generated from `protoPath` to a
   * stable on-disk location and returns it.
   *
   * The Java package is intentionally not reflected in the directory layout:
   * it is already declared inside the file, no consumer derives it from the
   * path, and class names are unique within a single `.proto`.
   */
  def materialize(
      workspace: AbsolutePath,
      protoPath: AbsolutePath,
      className: String,
      content: String,
  ): Option[AbsolutePath] =
    try {
      pathFor(workspace, protoPath, className).map { javaFile =>
        writeIfChanged(javaFile, content)
        javaFile
      }
    } catch {
      case NonFatal(e) =>
        scribe.debug(
          s"proto-java: failed to materialize generated outline for $className",
          e,
        )
        None
    }

  /**
   * Writes `outline` to the path it already names, for a compiler that can
   * only read a source from disk. No-op when the file is already that text.
   */
  def materialize(outline: ProtoOutlineFile): Unit =
    try writeIfChanged(AbsolutePath(outline.file), outline.text)
    catch {
      case NonFatal(e) =>
        scribe.debug(
          s"proto-java: failed to materialize ${outline.file}",
          e,
        )
    }

  /**
   * Where [[materialize]] would write `className.java`, without writing it.
   *
   * Derived from the proto alone, so it names an outline whether or not one is
   * on disk. A compiler that was handed the text from memory reports positions
   * against this path. For Scala 2 a file appears only when the user navigates
   * to it. For Scala 3 it is written when the compiler is built.
   */
  def pathFor(
      workspace: AbsolutePath,
      protoPath: AbsolutePath,
      className: String,
  ): Option[AbsolutePath] =
    protoPath.toRelativeInside(workspace).map { protoRelative =>
      workspace
        .resolve(Directories.dependencies)
        .resolve(rootDirName)
        .resolveZipPath(protoRelative.toNIO)
        .resolve(s"$className.java")
    }

  /**
   * Inverse of [[materialize]]: recovers the `.proto` file that the given
   * materialized Java file was generated from, or `None` when the path is not
   * a materialized proto outline. The proto path is the leading segments of
   * the path relative to the `proto-generated` root, up to and including the
   * first segment with a `.proto` extension.
   */
  def protoPathFor(
      workspace: AbsolutePath,
      path: AbsolutePath,
  ): Option[AbsolutePath] = {
    val root = workspace
      .resolve(Directories.dependencies)
      .resolve(rootDirName)
    for {
      relative <- path.toRelativeInside(root)
      segments = relative.toNIO.iterator().asScala.map(_.toString).toList
      protoIndex = segments.indexWhere(_.endsWith(".proto"))
      if protoIndex >= 0
    } yield segments
      .take(protoIndex + 1)
      .foldLeft(workspace)(_.resolve(_))
  }

  /**
   * Recreates the materialized outline for `outlineFile` when it has been
   * deleted (for example after a `.metals` clean, a `git clean`, or an editor
   * reload), so navigation, hover, and completion inside it keep working.
   *
   * `outlines` supplies the synthesized outlines for the proto `outlineFile`
   * was generated from; it is evaluated only when the file is actually missing.
   * No-op when the file still exists or no matching outline is found.
   */
  def regenerateIfMissing(
      workspace: AbsolutePath,
      outlineFile: AbsolutePath,
      protoPath: AbsolutePath,
      outlines: => Seq[VirtualTextDocument],
  ): Unit =
    if (!outlineFile.exists) {
      val className = outlineFile.filename.stripSuffix(".java")
      outlines
        .find(outline =>
          ProtoJavaVirtualFile
            .extractClassName(outline.uri().toString())
            .contains(className)
        )
        .foreach(outline =>
          materialize(workspace, protoPath, className, outline.text)
        )
    }

  private def writeIfChanged(file: AbsolutePath, content: String): Unit = {
    val changed =
      !file.exists || MD5.compute(file.toNIO) != MD5.compute(content)
    if (changed) {
      Files.createDirectories(file.toNIO.getParent)
      Files.write(file.toNIO, content.getBytes(StandardCharsets.UTF_8))
      // A compiler prefers a compiled class over a source of the same name
      // when `src.lastModified >= bin.lastModified`. Dating the outline 1970
      // makes the compiled class win. It carries more than Metals could infer
      // from the `.proto`. Scala 2 reaches the same result with a virtual
      // file. Its `lastModified` is 0.
      Files.setLastModifiedTime(file.toNIO, FileTime.fromMillis(0))
    }
  }
}
