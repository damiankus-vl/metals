package tests.mbt

import java.nio.file.Files

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtCompiledOnlyOutlineFiles
import scala.meta.io.AbsolutePath

class MbtCompiledOnlyOutlineFilesSuite extends munit.FunSuite {

  private def workspace(): AbsolutePath =
    AbsolutePath(Files.createTempDirectory("workspace"))

  test("materializes an outline keyed by the build target hash and binary name") {
    val ws = workspace()
    val result = MbtCompiledOnlyOutlineFiles.materialize(
      ws,
      "mbt://namespace/core",
      "com.example.Generated_Foo",
      "package com.example;\nclass Generated_Foo {}\n",
    )

    assert(result.isDefined, "expected a materialized file")
    assert(result.get.exists)
    assertEquals(
      result.get,
      MbtCompiledOnlyOutlineFiles.pathFor(
        ws,
        "mbt://namespace/core",
        "com.example.Generated_Foo",
      ),
    )
  }

  test("recovers the binary class name from a materialized path") {
    val ws = workspace()
    val javaFile = MbtCompiledOnlyOutlineFiles
      .materialize(
        ws,
        "mbt://namespace/core",
        "com.example.Generated_Foo",
        "content",
      )
      .get

    assertEquals(
      MbtCompiledOnlyOutlineFiles.originBinaryClassName(ws, javaFile),
      Some("com.example.Generated_Foo"),
    )
  }

  test("recovers a top-level (no-package) binary class name") {
    val ws = workspace()
    val javaFile = MbtCompiledOnlyOutlineFiles
      .materialize(ws, "mbt://namespace/core", "Foo", "content")
      .get

    assertEquals(
      MbtCompiledOnlyOutlineFiles.originBinaryClassName(ws, javaFile),
      Some("Foo"),
    )
  }

  test("originBinaryClassName is None for a path outside the compiled-only tree") {
    val ws = workspace()
    assertEquals(
      MbtCompiledOnlyOutlineFiles.originBinaryClassName(
        ws,
        ws.resolve("Foo.java"),
      ),
      None,
    )
  }

  test("originBuildTargetHash and originBinaryClassName agree on the same file") {
    val ws = workspace()
    val buildTargetId = "mbt://namespace/core"
    val javaFile = MbtCompiledOnlyOutlineFiles
      .materialize(ws, buildTargetId, "com.example.Generated_Foo", "content")
      .get

    assertEquals(
      MbtCompiledOnlyOutlineFiles.originBuildTargetHash(ws, javaFile),
      Some(scala.meta.internal.mtags.MD5.compute(buildTargetId)),
    )
    assertEquals(
      MbtCompiledOnlyOutlineFiles.originBinaryClassName(ws, javaFile),
      Some("com.example.Generated_Foo"),
    )
  }

  test("re-materializing identical content does not rewrite the file") {
    val ws = workspace()
    val first = MbtCompiledOnlyOutlineFiles
      .materialize(ws, "mbt://namespace/core", "Foo", "content")
      .get
    val firstModified = Files.getLastModifiedTime(first.toNIO)
    Thread.sleep(50)
    val second = MbtCompiledOnlyOutlineFiles
      .materialize(ws, "mbt://namespace/core", "Foo", "content")
      .get

    assertEquals(first, second)
    assertEquals(Files.getLastModifiedTime(second.toNIO), firstModified)
  }
}
