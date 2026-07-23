package tests.mbt

import scala.meta.internal.metals.mbt.importer.BazelMbtBuildSupport

import munit.FunSuite

class BazelMbtBuildSupportSuite extends FunSuite {

  test("module-id-from-target-label-main-workspace") {
    assertEquals(
      BazelMbtBuildSupport.moduleIdFromTargetLabel("//src/main/protobuf:x"),
      Some("src.main.protobuf:x:local"),
    )
  }

  test("module-id-from-target-label-external-apparent-repo") {
    assertEquals(
      BazelMbtBuildSupport.moduleIdFromTargetLabel(
        "@com_google_protobuf//:protobuf_java"
      ),
      Some("com_google_protobuf:protobuf_java:local"),
    )
  }

  test("module-id-from-target-label-external-canonical-repo") {
    assertEquals(
      BazelMbtBuildSupport.moduleIdFromTargetLabel(
        "@@protobuf+//:protobuf_java"
      ),
      Some("protobuf+:protobuf_java:local"),
    )
  }

  test("module-id-from-target-label-external-repo-with-package") {
    assertEquals(
      BazelMbtBuildSupport.moduleIdFromTargetLabel(
        "@com_google_protobuf//java/core:core"
      ),
      Some("com_google_protobuf.java.core:core:local"),
    )
  }

  test("module-id-from-target-label-rejects-non-label") {
    assertEquals(
      BazelMbtBuildSupport.moduleIdFromTargetLabel("not-a-label"),
      None,
    )
  }

  test("parse-target-label-main-workspace") {
    assertEquals(
      BazelMbtBuildSupport.parseTargetLabel("//src/main/protobuf:x"),
      Some(("", "src/main/protobuf", "x")),
    )
  }

  test("parse-target-label-external-apparent-repo") {
    assertEquals(
      BazelMbtBuildSupport.parseTargetLabel(
        "@com_google_protobuf//java/core:libcore.jar"
      ),
      Some(("com_google_protobuf", "java/core", "libcore.jar")),
    )
  }

  test("parse-target-label-external-canonical-repo") {
    assertEquals(
      BazelMbtBuildSupport.parseTargetLabel("@@protobuf+//:protobuf_java"),
      Some(("protobuf+", "", "protobuf_java")),
    )
  }

  test("parse-target-label-rejects-non-label") {
    assertEquals(BazelMbtBuildSupport.parseTargetLabel("not-a-label"), None)
  }
}
