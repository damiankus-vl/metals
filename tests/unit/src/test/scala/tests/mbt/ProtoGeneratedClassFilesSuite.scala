package tests.mbt

import java.nio.file.Files

import scala.meta.internal.metals.mbt.ProtoGeneratedClassFiles
import scala.meta.io.AbsolutePath

import munit.AnyFixture

class ProtoGeneratedClassFilesSuite extends munit.FunSuite {
  val workspace = new tests.TemporaryDirectoryFixture()
  override def munitFixtures: Seq[AnyFixture[_]] = List(workspace)

  private def classFile(binaryName: String): AbsolutePath =
    workspace()
      .resolve(".metals")
      .resolve("readonly")
      .resolve("dependencies")
      .resolve("proto-generated-classes")
      .resolve(s"$binaryName.class")

  test(
    "materializes a top-level and a nested class at their binary-name path"
  ) {
    val classes = Map(
      "com/example/proto/Company" -> Array[Byte](1, 2, 3),
      "com/example/proto/Company$Department" -> Array[Byte](4, 5, 6),
    )

    val dir = ProtoGeneratedClassFiles.materialize(workspace(), classes)

    assertEquals(
      dir,
      workspace()
        .resolve(".metals")
        .resolve("readonly")
        .resolve("dependencies")
        .resolve("proto-generated-classes"),
    )
    assertEquals(
      Files.readAllBytes(classFile("com/example/proto/Company").toNIO).toSeq,
      Seq[Byte](1, 2, 3),
    )
    assertEquals(
      Files
        .readAllBytes(classFile("com/example/proto/Company$Department").toNIO)
        .toSeq,
      Seq[Byte](4, 5, 6),
    )
  }

  test("does not rewrite an unchanged class file") {
    val binaryName = "com/example/proto/Company"
    ProtoGeneratedClassFiles.materialize(
      workspace(),
      Map(binaryName -> Array[Byte](1, 2, 3)),
    )
    val file = classFile(binaryName)
    val firstModified = Files.getLastModifiedTime(file.toNIO)
    Thread.sleep(50)

    ProtoGeneratedClassFiles.materialize(
      workspace(),
      Map(binaryName -> Array[Byte](1, 2, 3)),
    )

    assertEquals(Files.getLastModifiedTime(file.toNIO), firstModified)
  }

  test("rewrites a changed class file") {
    val binaryName = "com/example/proto/Company"
    ProtoGeneratedClassFiles.materialize(
      workspace(),
      Map(binaryName -> Array[Byte](1, 2, 3)),
    )

    ProtoGeneratedClassFiles.materialize(
      workspace(),
      Map(binaryName -> Array[Byte](7, 8, 9)),
    )

    assertEquals(
      Files.readAllBytes(classFile(binaryName).toNIO).toSeq,
      Seq[Byte](7, 8, 9),
    )
  }
}
