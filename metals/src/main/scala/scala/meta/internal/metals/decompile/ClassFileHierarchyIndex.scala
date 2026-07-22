package scala.meta.internal.metals.decompile

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}
import org.objectweb.asm.ClassReader

/**
 * Locates `.class` files for JVM symbols across classpath entries and walks
 * their compiled type hierarchy via ASM, without decompiling. Backs
 * navigation to a member inherited from a compiled class: given the classes
 * to start from, [[navigationTargets]] returns every declaring class, each
 * possibly in a different entry.
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
   * a hierarchy walk doesn't recompute the classpath for every class it
   * visits.
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

  // `FileIO.withJarFileSystem` resolves its filesystem through the JDK's own
  // `FileSystems.getFileSystem(uri)`, which -- even on a cache hit -- calls
  // `UnixPath.toRealPath` to recompute its lookup key, issuing real `realpath`
  // syscalls every time. `readClassFileFrom` calls this once per classpath
  // entry checked (hit or miss) for every class visited during a hierarchy
  // walk, so on a large classpath this dominates: profiling a single
  // goto-definition landed a third of all sampled CPU time in that syscall.
  // Caching the resolved root ourselves, keyed by jar path, skips the JDK's
  // expensive re-lookup on every subsequent access to the same jar.
  private val jarRootCache = new TrieMap[AbsolutePath, AbsolutePath]()

  private def jarRoot(jar: AbsolutePath): AbsolutePath =
    jarRootCache.getOrElseUpdate(jar, FileIO.jarRootPath(jar))

  private def readFromJar(
      jar: AbsolutePath,
      relativeClassPath: Path,
  ): Option[(String, Array[Byte])] =
    try {
      val classFile = jarRoot(jar).resolveZipPath(relativeClassPath)
      Option.when(classFile.exists)(
        classFile.toURI.toString -> Files.readAllBytes(classFile.toNIO)
      )
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
   * Walks the type hierarchy upward from `entryPoints` via bytecode,
   * returning a target for every class that declares `memberName` — each
   * method overload gets its own target, carrying its descriptor. Entry
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
   * Breadth-first walk of the type hierarchy: for each class in `frontier`,
   * record its targets for `memberName`, then continue into its supertypes.
   * `visited` avoids revisiting a class reachable via multiple paths;
   * `collectedTargets` accumulates in reverse discovery order.
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
   * Navigation targets contributed by a single class: one per overload of
   * `memberName` (carrying its descriptor so the caller can resolve that
   * overload's own source line), or one if it's a field. Empty if the class
   * declares no such member.
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
    if (overloadDescriptors.nonEmpty) {
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
  }

  /**
   * The 1-based source line of `memberName` on `classSymbol`, read from the
   * bytecode `LineNumberTable`. Used to jump to real source for a method that
   * exists only in compiled output (e.g. an annotation-processor-generated
   * accessor), where the line points back at the annotated field.
   * `methodDescriptor`, when given, picks
   * out one overload, so overloads sharing a name resolve to their own line
   * instead of all collapsing onto the first. `None` if the class isn't found
   * or has no debug line info.
   */
  def memberSourceLine(
      classSymbol: String,
      memberName: String,
      methodDescriptor: Option[String] = None,
  ): Option[Int] =
    readClassFile(classSymbol).flatMap { case (_, bytes) =>
      val visitor =
        new ClassFileMemberLineVisitor(memberName, methodDescriptor)
      // No SKIP_CODE: line numbers live in the Code attribute.
      new ClassReader(bytes).accept(visitor, ClassReader.SKIP_FRAMES)
      visitor.line
    }

  private def readClass(
      entries: Seq[AbsolutePath],
      classSymbol: String,
  ): Option[(String, ClassFileInfo)] =
    readClassFileFrom(entries, classSymbol).map { case (uri, bytes) =>
      val visitor = new ClassFileInfoVisitor
      new ClassReader(bytes).accept(
        visitor,
        ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES,
      )
      uri -> visitor.result
    }

  private def classLocation(uri: String): l.Location =
    new l.Location(
      uri,
      new l.Range(new l.Position(0, 0), new l.Position(0, 0)),
    )

  /** `com/example/Outer$Inner` becomes `com/example/Outer#Inner#`. */
  private def internalNameToSymbol(internalName: String): String =
    internalName.replace('$', '#') + "#"

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
