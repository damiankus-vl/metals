package tests.mbt

import java.io.File
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
    new InMemorySourceFile("User.java", outlinePath, outlineText)

  test("serves-a-source-that-is-not-on-disk") {
    val sources =
      sourcePath(Map(outlinePath -> outlineFile))
        .sources(PackageName("com.example.jproto"))
    assertEquals(sources.length, 1)
    val entry = sources.head
    // `ClassRepresentation.name` strips the extension and rejects a name
    // without one. A failure here means the file was named wrongly.
    assertEquals(entry.name, "User")
    assert(entry.file.isVirtual, "expected a source with no file behind it")
    assertNoDiff(new String(entry.file.unsafeToByteArray), outlineText)
  }

  test("is-recognizable-as-synthesized") {
    // `JavaPlatform.needCompile` reads this. It lets a compiled class win
    // over a synthesized stand-in.
    assert(outlineFile.isVirtual)
  }

  // Without the map the entry falls through to the file system, where nothing
  // is written. `AbstractFile.getFile` returns a `PlainFile` rather than null,
  // so the read fails later. The map is required, not an optimization.
  test("falls-back-to-the-file-system-when-nothing-is-in-memory") {
    val sources =
      sourcePath(Map.empty).sources(PackageName("com.example.jproto"))
    assertEquals(sources.length, 1)
    assert(
      !sources.head.file.isVirtual,
      "expected the file system entry, not a synthesized one",
    )
  }
}
