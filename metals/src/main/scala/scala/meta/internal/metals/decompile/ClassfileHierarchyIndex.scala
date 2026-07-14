package scala.meta.internal.metals.decompile

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.mutable
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}
import org.objectweb.asm.ClassReader

/**
 * Locates `.class` files for JVM symbols across a set of classpath jars and
 * walks their compiled type hierarchy via ASM, without decompiling anything.
 * Backs navigation to a member inherited from a compiled class: the caller
 * supplies the classes to start from (`hierarchyMemberTargets`) and gets back
 * every declaring class, each possibly living in a different jar.
 *
 * `classpathJars` is a supplier rather than a fixed collection because the
 * workspace classpath can change (e.g. after an import), so it is re-read on
 * every call.
 */
final class ClassfileHierarchyIndex(
    classpathJars: () => Iterator[AbsolutePath]
) {

  /** The `.class` location for a type symbol, from the first classpath jar that has it. */
  def classFileLocation(classSymbol: String): Option[l.Location] =
    readClassFile(classSymbol).map { case (uri, _) => classLocation(uri) }

  /**
   * Raw bytes and the jar-fs `.class` URI from the first classpath jar that
   * contains the class denoted by `classSymbol`.
   */
  def readClassFile(classSymbol: String): Option[(String, Array[Byte])] =
    classFileRelativePath(classSymbol).flatMap { relativeClassPath =>
      classpathJars()
        .filter(jar => jar.filename.endsWith(".jar") && jar.exists)
        .flatMap { jar =>
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
        }
        .nextOption()
    }

  /**
   * Walks the type hierarchy from `seeds` upward via bytecode, returning a
   * target for every class that declares a member named `memberName` (each
   * method overload gets its own target). Seeds and their ancestors may live
   * in different jars.
   */
  def hierarchyMemberTargets(
      seeds: Seq[String],
      memberName: String,
  ): Seq[(String, l.Location)] = {
    val visited = mutable.Set.empty[String]
    val queue = mutable.Queue.from(seeds)
    val targets = mutable.ListBuffer.empty[(String, l.Location)]
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (visited.add(current)) {
        readClass(current).foreach { case (uri, info) =>
          val overloads = info.methods.count(_._1 == memberName)
          if (overloads > 0)
            (0 until overloads).foreach { index =>
              val disambiguator = if (index == 0) "()." else s"(+$index)."
              targets +=
                (current + memberName + disambiguator) -> classLocation(uri)
            }
          else if (info.fields.contains(memberName))
            targets += (current + memberName + ".") -> classLocation(uri)
          (info.superName ++ info.interfaces).foreach(internalName =>
            queue.enqueue(internalNameToSymbol(internalName))
          )
        }
      }
    }
    targets.toList.distinctBy(target => (target._1, target._2.getUri()))
  }

  private def readClass(classSymbol: String): Option[(String, ClassfileInfo)] =
    readClassFile(classSymbol).map { case (uri, bytes) =>
      val visitor = new ClassfileInfoVisitor
      new ClassReader(bytes).accept(
        visitor,
        ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES,
      )
      uri -> visitor.result
    }

  private def classLocation(uri: String): l.Location =
    new l.Location(uri, new l.Range(new l.Position(0, 0), new l.Position(0, 0)))

  /** `com/example/Outer$Inner` becomes `com/example/Outer#Inner#`. */
  private def internalNameToSymbol(internalName: String): String =
    internalName.replace('$', '#') + "#"

  /**
   * Converts a SemanticDB type symbol into its `.class` entry path, mapping
   * nested classes to `$`-separated binary names, for example
   * `com/example/Outer#Inner#` becomes `com/example/Outer$Inner.class`.
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
