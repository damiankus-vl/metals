package tests.mbt

import java.nio.file.Files

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtBuild
import scala.meta.internal.metals.mbt.MbtCompiledOnlyOutlineFiles
import scala.meta.internal.metals.mbt.MbtCompiledOnlyOutlineProvider
import scala.meta.io.AbsolutePath

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes

class MbtCompiledOnlyOutlineProviderSuite extends munit.FunSuite {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val buildTargetId = "mbt://namespace/core"

  /** A trivial but real, CFR-decompilable top-level class. */
  private def writeClass(dir: java.nio.file.Path, internalName: String): Unit = {
    val cw = new ClassWriter(0)
    cw.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      internalName,
      null,
      "java/lang/Object",
      null,
    )
    cw.visitEnd()
    val classFile = dir.resolve(s"$internalName.class")
    Files.createDirectories(classFile.getParent)
    Files.write(classFile, cw.toByteArray())
  }

  private def buildWithClassDir(classDir: java.nio.file.Path): MbtBuild = {
    val f = Files.createTempFile("mbt", ".json")
    Files.writeString(
      f,
      s"""|{
          |  "namespaces": {
          |    "core": {
          |      "sources": [],
          |      "classDirectories": ["$classDir"]
          |    }
          |  }
          |}
          |""".stripMargin,
    )
    MbtBuild.fromFile(f)
  }

  private def workspace(): AbsolutePath =
    AbsolutePath(Files.createTempDirectory("workspace"))

  test("outlinesForPackage is empty when the feature is disabled") {
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Generated_Foo")
    val ws = workspace()
    val build = buildWithClassDir(classDir)

    val provider = new MbtCompiledOnlyOutlineProvider(
      () => build,
      ws,
      enabled = () => false,
    )

    val outlines =
      provider.outlinesForPackage(buildTargetId, "com/example", Nil)
    assert(outlines.isEmpty)
  }

  test("declining consent yields a package-only stub, not decompiled content") {
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Generated_Foo")
    val ws = workspace()
    val build = buildWithClassDir(classDir)

    val provider = new MbtCompiledOnlyOutlineProvider(
      () => build,
      ws,
      decompilationConsent = () => Future.successful(false),
    )

    val outline = provider
      .outlinesForPackage(buildTargetId, "com/example", Nil)
      .toList
      .head
    val text = outline.getCharContent(true).toString

    assertEquals(text, "package com.example;\n")
  }

  test("regenerateIfMissing recreates a deleted materialized outline") {
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Generated_Foo")
    val ws = workspace()
    val build = buildWithClassDir(classDir)

    val provider = new MbtCompiledOnlyOutlineProvider(() => build, ws)

    val outline = provider
      .outlinesForPackage(buildTargetId, "com/example", Nil)
      .toList
      .head
    // Force materialization the first time, same as the compiler reading it.
    outline.getCharContent(true)

    val javaFile =
      MbtCompiledOnlyOutlineFiles.pathFor(
        ws,
        buildTargetId,
        "com.example.Generated_Foo",
      )
    assert(javaFile.exists, "expected the outline to already be materialized")
    Files.delete(javaFile.toNIO)
    assert(!javaFile.exists)

    provider.regenerateIfMissing(buildTargetId, javaFile)

    assert(javaFile.exists, "expected the file to be recreated")
  }

  test("regenerateIfMissing is a no-op when the feature is disabled") {
    val classDir = Files.createTempDirectory("classdir")
    writeClass(classDir, "com/example/Generated_Foo")
    val ws = workspace()
    val build = buildWithClassDir(classDir)

    val provider = new MbtCompiledOnlyOutlineProvider(
      () => build,
      ws,
      enabled = () => false,
    )

    val javaFile =
      MbtCompiledOnlyOutlineFiles.pathFor(
        ws,
        buildTargetId,
        "com.example.Generated_Foo",
      )
    assert(!javaFile.exists)

    provider.regenerateIfMissing(buildTargetId, javaFile)

    assert(!javaFile.exists)
  }
}
