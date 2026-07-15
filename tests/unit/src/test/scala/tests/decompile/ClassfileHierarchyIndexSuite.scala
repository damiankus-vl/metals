package tests.decompile

import java.nio.file.Files
import java.nio.file.Path

import scala.meta.internal.metals.decompile.ClassfileHierarchyIndex
import scala.meta.io.AbsolutePath

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes

class ClassfileHierarchyIndexSuite extends munit.FunSuite {

  private def writeClass(
      dir: Path,
      internalName: String,
      superName: String,
      methodName: String,
      sourceLine: Option[Int] = None,
  ): Unit = {
    val cw = new ClassWriter(0)
    cw.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      internalName,
      null,
      superName,
      null,
    )
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, methodName, "()V", null, null)
    sourceLine.foreach { line =>
      mv.visitCode()
      val label = new Label()
      mv.visitLabel(label)
      mv.visitLineNumber(line, label)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(0, 1)
    }
    mv.visitEnd()
    cw.visitEnd()
    val classFile = dir.resolve(s"$internalName.class")
    Files.createDirectories(classFile.getParent)
    Files.write(classFile, cw.toByteArray())
  }

  test("reads-class-and-member-from-directory") {
    val dir = Files.createTempDirectory("classdir")
    writeClass(dir, "com/example/Foo", "java/lang/Object", "bar")
    val index = new ClassfileHierarchyIndex(() => Iterator(AbsolutePath(dir)))

    val targets = index.hierarchyMemberTargets(Seq("com/example/Foo#"), "bar")
    assertEquals(targets.map(_._1), List("com/example/Foo#bar()."))

    val location = index.classFileLocation("com/example/Foo#")
    assert(
      location.isDefined,
      "expected a class-file location from a directory",
    )
    assert(
      location.get.getUri().endsWith("com/example/Foo.class"),
      s"unexpected uri: ${location.map(_.getUri())}",
    )
  }

  test("walks-hierarchy-across-directory-entries") {
    // The member is declared on a compiled ancestor in a different directory,
    // reached by following superName from the seed's directory.
    val childDir = Files.createTempDirectory("child")
    val parentDir = Files.createTempDirectory("parent")
    writeClass(childDir, "com/example/Child", "com/example/Base", "childOnly")
    writeClass(parentDir, "com/example/Base", "java/lang/Object", "inherited")
    val index =
      new ClassfileHierarchyIndex(() =>
        Iterator(AbsolutePath(childDir), AbsolutePath(parentDir))
      )

    val targets =
      index.hierarchyMemberTargets(Seq("com/example/Child#"), "inherited")
    assertEquals(targets.map(_._1), List("com/example/Base#inherited()."))
  }

  test("reads-member-source-line-from-bytecode") {
    val dir = Files.createTempDirectory("linedir")
    writeClass(
      dir,
      "com/example/Bean",
      "java/lang/Object",
      "getName",
      sourceLine = Some(42),
    )
    val index = new ClassfileHierarchyIndex(() => Iterator(AbsolutePath(dir)))

    assertEquals(
      index.memberSourceLine("com/example/Bean#", "getName"),
      Some(42),
    )
    assertEquals(index.memberSourceLine("com/example/Bean#", "absent"), None)
  }
}
