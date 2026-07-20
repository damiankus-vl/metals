package scala.meta.internal.metals.mbt

import java.nio.file.Files
import java.nio.file.Paths

import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

/**
 * Materializes turbine's compiled bytecode for proto-generated Java classes
 * (see [[MbtWorkspaceSymbolProvider.protoGeneratedClassBytes]]) into real
 * `.class` files on disk, so they can be added to a Scala build target's
 * presentation-compiler classpath. Unlike the Java presentation compiler --
 * which resolves these classes directly from turbine's in-memory bytes via a
 * custom `JavaFileManager` -- the Scala presentation compiler only knows how
 * to read a real classpath, hence the need to materialize.
 */
object ProtoGeneratedClassFiles {

  /** Subdirectory of `.metals/readonly/dependencies` for compiled classes. */
  private val rootDirName = "proto-generated-classes"

  private def root(workspace: AbsolutePath): AbsolutePath =
    workspace.resolve(Directories.dependencies).resolve(rootDirName)

  /**
   * Writes `classes` (JVM binary name -> compiled bytecode) under a stable
   * on-disk directory laid out the way a classpath directory entry is
   * expected to be (`<root>/<pkg>/<Simple>[$Nested].class`), and returns
   * that directory.
   */
  def materialize(
      workspace: AbsolutePath,
      classes: Map[String, Array[Byte]],
  ): AbsolutePath = {
    val dir = root(workspace)
    for ((binaryName, bytes) <- classes) {
      val classFile = dir.resolveZipPath(Paths.get(s"$binaryName.class"))
      writeIfChanged(classFile, bytes)
    }
    dir
  }

  private def writeIfChanged(file: AbsolutePath, bytes: Array[Byte]): Unit = {
    val current =
      if (file.exists) Some(Files.readAllBytes(file.toNIO)) else None
    if (!current.exists(java.util.Arrays.equals(_, bytes))) {
      Files.createDirectories(file.toNIO.getParent)
      Files.write(file.toNIO, bytes)
    }
  }
}
