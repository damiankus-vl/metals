package tests.decompile

import java.net.URI
import java.nio.file.Files
import java.nio.file.Path

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import scala.meta.internal.metals.decompile.NavigationTargetProvider
import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.io.AbsolutePath
import scala.meta.pc.Language

import org.eclipse.{lsp4j => l}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes

class NavigationTargetProviderSuite extends munit.FunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  /**
   * Writes a class whose method `methodName` carries a `LineNumberTable`
   * pointing at `sourceLine`, mimicking a compiled-only accessor (e.g. a Lombok
   * `@Getter`) whose bytecode line points back at the annotated field.
   */
  private def writeClass(
      dir: Path,
      internalName: String,
      methodName: String,
      sourceLine: Int,
  ): Unit = {
    val cw = new ClassWriter(0)
    cw.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      internalName,
      null,
      "java/lang/Object",
      null,
    )
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, methodName, "()V", null, null)
    mv.visitCode()
    val label = new Label()
    mv.visitLabel(label)
    mv.visitLineNumber(sourceLine, label)
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(0, 1)
    mv.visitEnd()
    cw.visitEnd()
    val classFile = dir.resolve(s"$internalName.class")
    Files.createDirectories(classFile.getParent)
    Files.write(classFile, cw.toByteArray())
  }

  test("member-target-lands-on-lombok-field-identifier") {
    val classDir = Files.createTempDirectory("classdir")
    // The getter's bytecode line points at the field it accesses (line 3).
    writeClass(classDir, "com/example/Bean", "getValue", sourceLine = 3)

    val sourceFile = Files.createTempFile("Bean", ".java")
    Files.writeString(
      sourceFile,
      List(
        "package com.example;",
        "class Bean {",
        "  @Getter private final String value = null;",
        "}",
      ).mkString("\n"),
    )
    val source = AbsolutePath(sourceFile)

    val provider = new NavigationTargetProvider(
      () => Iterator(AbsolutePath(classDir)),
      _ => None,
      classSymbol => Option.when(classSymbol == "com/example/Bean#")(source),
    )

    val targets = Await.result(
      provider.classHierarchyTargets("com/example/Bean#getValue#"),
      10.seconds,
    )

    assertEquals(targets.size, 1)
    val location = targets.head._2
    assertEquals(location.getUri(), source.toURI.toString)
    // Selects the `value` field identifier, not the whole line, so the editor
    // can collapse it with an identical result from a full Java language server.
    val expectedColumn =
      "  @Getter private final String value = null;".indexOf("value = null")
    assertEquals(
      location.getRange(),
      new l.Range(
        new l.Position(2, expectedColumn),
        new l.Position(2, expectedColumn + "value".length),
      ),
    )
  }

  test("entry-point-from-proto-outline-ignoring-comment-decoys") {
    // The owner isn't on the classpath, so the walk's entry point comes from
    // its synthesized outline's supertypes. A doc comment mentions "class Foo"
    // before the real declaration; the scan must skip the comment and read the
    // true `extends`, otherwise the inherited member is unreachable.
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Base", "getValue", sourceLine = 5)

    val outlineText =
      List(
        "package com.example;",
        "/** See {@link Foo}: the class Foo replacement lives here. */",
        "@Deprecated", "public final class Foo extends com.example.Base {", "}",
      ).mkString("\n")
    val outline = VirtualTextDocument(
      URI.create("file:///Foo.java"),
      Language.JAVA,
      outlineText,
      Seq("com/example"),
      Seq("com/example/Foo#"),
    )

    val provider = new NavigationTargetProvider(
      () => Iterator(AbsolutePath(classDir)),
      classSymbol => Option.when(classSymbol == "com/example/Foo#")(outline),
      _ => None,
    )

    val targets = Await.result(
      provider.classHierarchyTargets("com/example/Foo#getValue#"),
      10.seconds,
    )

    assertEquals(targets.map(_._1), List("com/example/Base#getValue()."))
    assert(
      targets.head._2.getUri().endsWith("com/example/Base.class"),
      s"expected the Base .class location, got: ${targets.head._2.getUri()}",
    )
  }

  test("entry-point-from-proto-outline-with-bracket-generics") {
    // Square-bracket type arguments (Scala-style, e.g. `Base[String, Int]`)
    // must not be mistaken for a second, comma-separated supertype the way a
    // Java `<...>` generic already isn't.
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Base", "getValue", sourceLine = 5)
    writeClass(classDir, "com/example/Other", "getValue", sourceLine = 9)

    val outlineText =
      List(
        "package com.example;",
        "public final class Foo extends com.example.Base[String, Int], com.example.Other {",
        "}",
      ).mkString("\n")
    val outline = VirtualTextDocument(
      URI.create("file:///Foo.java"),
      Language.JAVA,
      outlineText,
      Seq("com/example"),
      Seq("com/example/Foo#"),
    )

    val provider = new NavigationTargetProvider(
      () => Iterator(AbsolutePath(classDir)),
      classSymbol => Option.when(classSymbol == "com/example/Foo#")(outline),
      _ => None,
    )

    val targets = Await.result(
      provider.classHierarchyTargets("com/example/Foo#getValue#"),
      10.seconds,
    )

    assertEquals(
      targets.map(_._1).toSet,
      Set("com/example/Base#getValue().", "com/example/Other#getValue()."),
    )
  }
}
