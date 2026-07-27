package scala.meta.internal.metals.mbt

import java.nio.file.Files
import java.nio.file.Paths

import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

/**
 * Writes turbine's compiled bytecode for proto-generated Java classes (see
 * [[MbtWorkspaceSymbolProvider.protoGeneratedClassBytes]]) to real `.class`
 * files, so they can join a Scala build target's presentation-compiler
 * classpath. The Java presentation compiler reads these classes directly
 * from turbine's in-memory bytes via a custom `JavaFileManager`; the Scala
 * one only knows how to read a real classpath, hence the need to
 * materialize.
 */
object ProtoGeneratedClassFiles {

  /** Subdirectory of `.metals/readonly/dependencies` for compiled classes. */
  private val rootDirName = "proto-generated-classes"

  /** The stable on-disk directory `materialize` writes classes under. */
  def directory(workspace: AbsolutePath): AbsolutePath =
    workspace.resolve(Directories.dependencies).resolve(rootDirName)

  /**
   * Writes `classes` (JVM binary name -> compiled bytecode) as
   * `<root>/<pkg>/<Simple>[$Nested].class`, the layout a classpath directory
   * entry expects, and returns that directory.
   *
   * Class files left over from a previous call are deleted: this directory is
   * a classpath entry, so a class whose message was renamed or removed would
   * otherwise keep resolving forever.
   */
  def materialize(
      workspace: AbsolutePath,
      classes: Map[String, Array[Byte]],
  ): AbsolutePath = {
    val dir = directory(workspace)
    val current = classes.map { case (binaryName, bytes) =>
      val classFile = dir.resolveZipPath(Paths.get(s"$binaryName.class"))
      writeIfChanged(classFile, bytes)
      classFile
    }.toSet
    deleteStaleClassFiles(dir, current)
    dir
  }

  private def deleteStaleClassFiles(
      dir: AbsolutePath,
      current: Set[AbsolutePath],
  ): Unit =
    for {
      file <- dir.listRecursive
      if file.isFile && file.filename.endsWith(".class")
      if !current.contains(file)
    } Files.deleteIfExists(file.toNIO)

  private def writeIfChanged(file: AbsolutePath, bytes: Array[Byte]): Unit = {
    val current =
      if (file.exists) Some(Files.readAllBytes(file.toNIO)) else None
    if (!current.exists(java.util.Arrays.equals(_, bytes))) {
      Files.createDirectories(file.toNIO.getParent)
      Files.write(file.toNIO, bytes)
    }
  }
}
