package scala.meta.internal.metals.decompile

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.annotation.tailrec
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}
import org.objectweb.asm.ClassReader

/**
 * Locates `.class` files for JVM symbols across a set of classpath entries and
 * walks their compiled type hierarchy via ASM, without decompiling anything.
 * Backs navigation to a member inherited from a compiled class: the caller
 * supplies the classes to start from (`hierarchyMemberTargets`) and gets back
 * every declaring class, each possibly living in a different entry.
 *
 * Entries may be jars (dependency jars, Bazel `lib*.jar` outputs) or class
 * directories (Maven `target/classes`, Gradle `build/classes`). It is a
 * supplier rather than a fixed collection because the workspace classpath can
 * change (e.g. after an import), so it is re-read on every call.
 */
final class ClassfileHierarchyIndex(
    classpathEntries: () => Iterator[AbsolutePath]
) {

  /** The `.class` location for a type symbol, from the first entry that has it. */
  def classFileLocation(classSymbol: String): Option[l.Location] = {
    val result =
      readClassFile(classSymbol).map { case (uri, _) => classLocation(uri) }
    result
  }

  /**
   * Raw bytes and the `.class` URI from the first classpath entry (jar or class
   * directory) that contains the class denoted by `classSymbol`.
   */
  def readClassFile(classSymbol: String): Option[(String, Array[Byte])] = {
    val result =
      classFileRelativePath(classSymbol).flatMap { relativeClassPath =>
        classpathEntries()
          .filter(_.exists)
          .flatMap(entry => readFromEntry(entry, relativeClassPath))
          .nextOption()
      }
    result
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

  /**
   * Walks the type hierarchy from `seeds` upward via bytecode, returning a
   * target for every class that declares a member named `memberName` (each
   * method overload gets its own target). Seeds and their ancestors may live
   * in different jars.
   */
  def hierarchyMemberTargets(
      seeds: Seq[String],
      memberName: String,
  ): Seq[(String, l.Location)] =
    walk(seeds.toList, memberName, Set.empty, Nil)
      .distinctBy { case (symbol, location) => (symbol, location.getUri()) }

  /**
   * Breadth-first traversal of the type hierarchy: for each class in `frontier`
   * record the targets it declares for `memberName`, then continue into its
   * supertypes. `visited` prevents revisiting a class reachable by multiple
   * paths; `acc` collects targets in reverse discovery order.
   */
  @tailrec
  private def walk(
      frontier: List[String],
      memberName: String,
      visited: Set[String],
      acc: List[(String, l.Location)],
  ): List[(String, l.Location)] =
    frontier match {
      case Nil => acc.reverse
      case current :: rest if visited(current) =>
        walk(rest, memberName, visited, acc)
      case current :: rest =>
        readClass(current) match {
          case None => walk(rest, memberName, visited + current, acc)
          case Some((uri, info)) =>
            val found = memberTargets(current, memberName, uri, info)
            val supertypes =
              (info.superName ++ info.interfaces).map(internalNameToSymbol)
            walk(
              rest ++ supertypes,
              memberName,
              visited + current,
              found reverse_::: acc,
            )
        }
    }

  /**
   * The navigation targets contributed by a single class: one per overload of a
   * method named `memberName`, or one for a field of that name. Empty when the
   * class declares no such member.
   */
  private def memberTargets(
      classSymbol: String,
      memberName: String,
      uri: String,
      info: ClassfileInfo,
  ): List[(String, l.Location)] = {
    val overloads = info.methods.count { case (name, _) => name == memberName }
    val result = if (overloads > 0) {
      (0 until overloads).map { index =>
        val disambiguator = if (index == 0) "()." else s"(+$index)."
        (classSymbol + memberName + disambiguator) -> classLocation(uri)
      }.toList
    } else if (info.fields.contains(memberName)) {
      List((classSymbol + memberName + ".") -> classLocation(uri))
    } else {
      Nil
    }
    result
  }

  /**
   * The 1-based source line of the method `memberName` on `classSymbol`, read
   * from its bytecode `LineNumberTable`. Used to jump to real source for a
   * method that exists only in compiled output (e.g. a Lombok accessor), where
   * the line points back at the annotated field. `None` when the class isn't
   * found or carries no debug line information.
   */
  def memberSourceLine(
      classSymbol: String,
      memberName: String,
  ): Option[Int] = {
    val result =
      readClassFile(classSymbol).flatMap { case (_, bytes) =>
        val visitor = new ClassfileMemberLineVisitor(memberName)
        // No SKIP_CODE: line numbers live in the Code attribute.
        new ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
        visitor.line
      }
    result
  }

  private def readClass(
      classSymbol: String
  ): Option[(String, ClassfileInfo)] = {
    val result =
      readClassFile(classSymbol).map { case (uri, bytes) =>
        val visitor = new ClassfileInfoVisitor
        new ClassReader(bytes).accept(
          visitor,
          ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES,
        )
        uri -> visitor.result
      }
    result
  }

  private def classLocation(uri: String): l.Location = {
    val result =
      new l.Location(
        uri,
        new l.Range(new l.Position(0, 0), new l.Position(0, 0)),
      )
    result
  }

  /** `com/example/Outer$Inner` becomes `com/example/Outer#Inner#`. */
  private def internalNameToSymbol(internalName: String): String = {
    val result = internalName.replace('$', '#') + "#"
    result
  }

  /**
   * Converts a SemanticDB type symbol into its `.class` entry path, mapping
   * nested classes to `$`-separated binary names, for example
   * `com/example/Outer#Inner#` becomes `com/example/Outer$Inner.class`.
   */
  private def classFileRelativePath(symbol: String): Option[Path] = {
    val trimmed = symbol.stripSuffix("#").stripSuffix(".")
    val result =
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
    result
  }
}
