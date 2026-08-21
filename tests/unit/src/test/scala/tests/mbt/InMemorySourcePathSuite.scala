package tests.mbt

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.nio.file.Paths
import java.{util => ju}

import scala.jdk.CollectionConverters._
import scala.reflect.io.AbstractFile
import scala.tools.nsc.InMemorySourceFile
import scala.tools.nsc.LogicalSourcePath
import scala.tools.nsc.ParsedLogicalPackage
import scala.tools.nsc.classpath.PackageName

/**
 * The compiler reads a source path entry through
 * [[scala.tools.nsc.LogicalPackage.sourceFile]]. A source Metals synthesizes
 * can then be served without reaching the file system.
 */
class InMemorySourcePathSuite extends munit.FunSuite {

  private val outlinePath =
    "/does/not/exist/proto-generated/model.proto/User.java"
  private val outlineText =
    """|package com.example.jproto;
       |public final class User {
       |  public String getName() { return null; }
       |}
       |""".stripMargin

  private def sourcePath(
      inMemory: Map[String, AbstractFile]
  ): LogicalSourcePath = {
    val packages: ju.Map[String, ju.Set[Path]] =
      Map("com/example/jproto/" -> Set(Paths.get(outlinePath)).asJava).asJava
    new LogicalSourcePath(
      Seq.empty[File],
      ParsedLogicalPackage.fromMbtIndex(packages, inMemory),
    )
  }

  private def outlineFile =
    new InMemorySourceFile(
      name = "User.java",
      path = outlinePath,
      text = outlineText,
    )

  test("serves-a-source-that-is-not-on-disk") {
    val sources =
      sourcePath(Map(outlinePath -> outlineFile))
        .sources(PackageName("com.example.jproto"))
    assertEquals(sources.length, 1)
    val entry = sources.headOption
      .getOrElse(fail("expected the synthesized source to be served"))
    // `ClassRepresentation.name` strips the extension and rejects a name
    // without one. A failure here means the file was named wrongly.
    assertEquals(entry.name, "User")
    // `JavaPlatform.needCompile` reads `isVirtual`, and it is what lets a
    // compiled class win over a synthesized stand-in.
    assert(entry.file.isVirtual, "expected a source with no file behind it")
    assertNoDiff(
      new String(entry.file.unsafeToByteArray, StandardCharsets.UTF_8),
      outlineText,
    )
  }

  // A comment copied from the `.proto` can carry a character UTF-8 spends more
  // than one byte on. `sizeOption` has to answer in bytes, since that is what
  // sizes the buffer a reader fills.
  test("serves-a-source-that-is-not-all-ascii") {
    val text =
      """|package com.example.jproto;
         |// ééé
         |public final class User {}
         |""".stripMargin
    val file = new InMemorySourceFile(
      name = "User.java",
      path = outlinePath,
      text = text,
    )
    // `toByteArray` sizes its buffer from `sizeOption`, so a character count
    // there truncates the tail of the outline.
    assertNoDiff(new String(file.toByteArray, StandardCharsets.UTF_8), text)
    // 62 characters, and 65 bytes because each `é` costs two. Written out
    // rather than read back off the file, which would agree with whatever
    // `sizeOption` came to answer.
    assertEquals(text.length, 62)
    assertEquals(file.sizeOption, Some(65))
  }

  // Without the map the entry falls through to the file system, where nothing
  // is written. `AbstractFile.getFile` returns a `PlainFile` rather than null,
  // so the read fails later. The map is required, not an optimization.
  test("falls-back-to-the-file-system-when-nothing-is-in-memory") {
    val sources =
      sourcePath(Map.empty).sources(PackageName("com.example.jproto"))
    assertEquals(sources.length, 1)
    val entry = sources.headOption
      .getOrElse(fail("expected the file system entry to be served"))
    assert(
      !entry.file.isVirtual,
      "expected the file system entry, not a synthesized one",
    )
  }
}
