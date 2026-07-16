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
 * supplies the classes to start from (`navigationTargets`) and gets back
 * every declaring class, each possibly living in a different entry.
 *
 * Entries may be jars (dependency jars, Bazel `lib*.jar` outputs) or class
 * directories (Maven `target/classes`, Gradle `build/classes`). It is a
 * supplier rather than a fixed collection because the workspace classpath can
 * change (e.g. after an import), so it is re-read on every call.
 */
final class ClassFileHierarchyIndex(
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
   * directory) that contains the class denoted by `typeSymbol`.
   */
  def readClassFile(classSymbol: String): Option[(String, Array[Byte])] =
    readClassFileFrom(classpathSnapshot(), classSymbol)

  /**
   * A materialized snapshot of the (existing) classpath entries. Taken once per
   * navigation so a hierarchy walk doesn't rebuild the whole entry chain — jars,
   * java compiler classpath, and class directories — for every class it visits.
   */
  private def classpathSnapshot(): Seq[AbsolutePath] =
    classpathEntries().filter(_.exists).toVector

  private def readClassFileFrom(
      entries: Seq[AbsolutePath],
      classSymbol: String,
  ): Option[(String, Array[Byte])] = {
    val result =
      classFileRelativePath(classSymbol).flatMap { relativeClassPath =>
        entries.iterator
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
   * Walks the type hierarchy from `entryPoints` upward via bytecode, returning
   * a target for every class that declares a member named `memberName` (each
   * method overload gets its own target, carrying its descriptor). Entry
   * points and their ancestors may live in different jars.
   */
  def navigationTargets(
      entryPoints: Seq[String],
      memberName: String,
  ): Seq[NavigationTarget] =
    walk(classpathSnapshot(), entryPoints.toList, memberName, Set.empty, Nil)
      .distinctBy(target =>
        (
          target.memberSymbol,
          target.classLocation.getUri(),
          target.methodDescriptor,
        )
      )

  /**
   * Breadth-first traversal of the type hierarchy: for each class in `frontier`
   * record the navigation targets it declares for `memberName`, then continue into its
   * supertypes. `visited` prevents revisiting a class reachable by multiple
   * paths; `collectedTargets` collects targets in reverse discovery order.
   */
  @tailrec
  private def walk(
      entries: Seq[AbsolutePath],
      frontier: List[String],
      memberName: String,
      visited: Set[String],
      collectedTargets: List[NavigationTarget],
  ): List[NavigationTarget] =
    frontier match {
      case Nil => collectedTargets.reverse
      case current :: rest if visited(current) =>
        walk(entries, rest, memberName, visited, collectedTargets)
      case current :: rest =>
        readClass(entries, current) match {
          case None =>
            walk(entries, rest, memberName, visited + current, collectedTargets)
          case Some((uri, info)) =>
            val found = memberNavigationTargets(current, memberName, uri, info)
            val supertypes =
              (info.superName ++ info.interfaces).map(internalNameToSymbol)
            walk(
              entries,
              rest ++ supertypes,
              memberName,
              visited + current,
              found reverse_::: collectedTargets,
            )
        }
    }

  /**
   * The navigation targets contributed by a single class: one per overload of a
   * method named `memberName` (each carrying its JVM descriptor so the caller
   * can resolve that overload's own source line), or one for a field of that
   * name. Empty when the class declares no such member.
   */
  private def memberNavigationTargets(
      classSymbol: String,
      memberName: String,
      uri: String,
      info: ClassFileInfo,
  ): List[NavigationTarget] = {
    val overloadDescriptors = info.methods.collect {
      case MethodInfo(name, descriptor) if name == memberName => descriptor
    }
    val result = if (overloadDescriptors.nonEmpty) {
      overloadDescriptors.zipWithIndex.map { case (descriptor, index) =>
        val disambiguator = if (index == 0) "()." else s"(+$index)."
        NavigationTarget(
          classSymbol + memberName + disambiguator,
          classLocation(uri),
          Some(descriptor),
        )
      }.toList
    } else if (info.fields.contains(memberName)) {
      List(
        NavigationTarget(
          classSymbol + memberName + ".",
          classLocation(uri),
          None,
        )
      )
    } else {
      Nil
    }
    result
  }

  /**
   * The 1-based source line of the method `memberName` on `typeSymbol`, read
   * from its bytecode `LineNumberTable`. Used to jump to real source for a
   * method that exists only in compiled output (e.g. a Lombok accessor), where
   * the line points back at the annotated field. When `methodDescriptor` is
   * given, only that overload is matched, so overloads sharing a name resolve
   * to their own lines instead of all collapsing onto the first. `None` when the
   * class isn't found or carries no debug line information.
   */
  def memberSourceLine(
      classSymbol: String,
      memberName: String,
      methodDescriptor: Option[String] = None,
  ): Option[Int] = {
    val result =
      readClassFile(classSymbol).flatMap { case (_, bytes) =>
        val visitor =
          new ClassFileMemberLineVisitor(memberName, methodDescriptor)
        // No SKIP_CODE: line numbers live in the Code attribute.
        new ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
        visitor.line
      }
    result
  }

  private def readClass(
      entries: Seq[AbsolutePath],
      classSymbol: String,
  ): Option[(String, ClassFileInfo)] = {
    val result =
      readClassFileFrom(entries, classSymbol).map { case (uri, bytes) =>
        val visitor = new ClassFileInfoVisitor
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
