package scala.meta.internal.metals.decompile

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}

/**
 * Locates `.class` files for JVM symbols across classpath entries, without
 * decompiling.
 *
 * Entries can be jars or class directories. Classpath entries are supplied as
 * a function rather than a fixed collection because the workspace classpath
 * can change (e.g. after an import), so it's re-read on every call.
 */
final class ClassFileHierarchyIndex(
    classpathEntries: () => Iterator[AbsolutePath]
) {

  /** The `.class` location for a type symbol, from the first entry that has it. */
  def classFileLocation(classSymbol: String): Option[l.Location] =
    readClassFile(classSymbol).map { case (uri, _) => classLocation(uri) }

  /**
   * Raw bytes and `.class` URI from the first classpath entry (jar or
   * directory) that contains `classSymbol`.
   */
  def readClassFile(classSymbol: String): Option[(String, Array[Byte])] =
    readClassFileFrom(classpathSnapshot(), classSymbol)

  /**
   * Snapshot of the existing classpath entries, taken once per navigation so
   * repeated lookups don't recompute the classpath every time.
   */
  private def classpathSnapshot(): Seq[AbsolutePath] =
    classpathEntries().filter(_.exists).toVector

  private def readClassFileFrom(
      entries: Seq[AbsolutePath],
      classSymbol: String,
  ): Option[(String, Array[Byte])] =
    classFileRelativePath(classSymbol).flatMap { relativeClassPath =>
      entries.iterator
        .flatMap(entry => readFromEntry(entry, relativeClassPath))
        .nextOption()
    }

  private def readFromEntry(
      entry: AbsolutePath,
      relativeClassPath: Path,
  ): Option[(String, Array[Byte])] =
    if (entry.filename.endsWith(".jar")) readFromJar(entry, relativeClassPath)
    else if (entry.isDirectory) readFromDirectory(entry, relativeClassPath)
    else None

  private def readFromJar(
      jar: AbsolutePath,
      relativeClassPath: Path,
  ): Option[(String, Array[Byte])] =
    try {
      FileIO.withJarFileSystem(jar, create = false) { root =>
        val classFile = root.resolveZipPath(relativeClassPath)
        Option.when(classFile.exists)(
          classFile.toURI.toString -> Files.readAllBytes(classFile.toNIO)
        )
      }
    } catch {
      case NonFatal(_) => None
    }

  private def readFromDirectory(
      dir: AbsolutePath,
      relativeClassPath: Path,
  ): Option[(String, Array[Byte])] = {
    val classFile = dir.toNIO.resolve(relativeClassPath)
    Option.when(Files.exists(classFile))(
      classFile.toUri.toString -> Files.readAllBytes(classFile)
    )
  }

  private def classLocation(uri: String): l.Location =
    new l.Location(
      uri,
      new l.Range(new l.Position(0, 0), new l.Position(0, 0)),
    )

  /**
   * Converts a SemanticDB symbol into its `.class` entry path, mapping nested
   * classes to `$`-separated binary names: `com/example/Outer#Inner#`
   * becomes `com/example/Outer$Inner.class`.
   */
  private def classFileRelativePath(symbol: String): Option[Path] = {
    val trimmed = symbol.stripSuffix("#").stripSuffix(".")
    if (trimmed.isEmpty || trimmed.endsWith("/")) None
    else {
      val lastSlash = trimmed.lastIndexOf('/')
      val (packagePrefix, className) =
        if (lastSlash < 0) ("", trimmed)
        else
          (
            trimmed.substring(0, lastSlash + 1),
            trimmed.substring(lastSlash + 1),
          )
      val binaryName = className.replace('#', '$').replace('.', '$')
      Some(Paths.get(s"$packagePrefix$binaryName.class"))
    }
  }
}
