package tests.mbt

import scala.meta.internal.metals.mbt.importer.BazelTargetsXmlDump

import munit.FunSuite

class BazelTargetsXmlDumpSuite extends FunSuite {

  // Mimics `deps(set(//app:app))`: a known java_library depending directly on
  // a main-workspace java_proto_library and an external-repo alias (the real
  // shape confirmed against a live Bazel workspace: `@com_google_protobuf//:protobuf_java`
  // is itself an `alias` rule, not a `java_library`). The java_proto_library
  // and the alias each point further at nodes with no jar output of their own
  // (a bare `proto_library`, and an unresolved-in-this-dump repo-internal
  // target) -- neither of those should show up as extra dependency labels.
  private val xml =
    """|<query>
       |  <rule class="java_library" name="//app:app">
       |    <rule-input name="//protos:foo_java_proto"/>
       |    <rule-input name="@com_google_protobuf//:protobuf_java"/>
       |  </rule>
       |  <rule class="java_proto_library" name="//protos:foo_java_proto">
       |    <rule-input name="//protos:foo_proto"/>
       |  </rule>
       |  <rule class="proto_library" name="//protos:foo_proto">
       |  </rule>
       |  <rule class="alias" name="@com_google_protobuf//:protobuf_java">
       |    <rule-input name="@com_google_protobuf//java/core:core"/>
       |    <label name="actual" value="@com_google_protobuf//java/core:core"/>
       |  </rule>
       |  <rule class="java_library" name="@com_google_protobuf//java/core:core">
       |    <rule-output name="@com_google_protobuf//java/core:libcore.jar"/>
       |    <rule-output name="@com_google_protobuf//java/core:libcore-src.jar"/>
       |  </rule>
       |</query>
       |""".stripMargin

  test("extra-dependency-labels-finds-proto-library-and-external-alias") {
    val dump = new BazelTargetsXmlDump(xml)
    val knownTargets = Set("//app:app")
    val reachable = dump.reachableLabels(knownTargets.toList)
    val extra = dump.extraDependencyLabels(reachable, knownTargets)
    assertEquals(
      extra,
      Set(
        "//protos:foo_java_proto",
        "@com_google_protobuf//:protobuf_java",
        "@com_google_protobuf//java/core:core",
      ),
    )
  }

  test("extra-dependency-labels-excludes-bare-proto-library") {
    val dump = new BazelTargetsXmlDump(xml)
    val knownTargets = Set("//app:app")
    val reachable = dump.reachableLabels(knownTargets.toList)
    val extra = dump.extraDependencyLabels(reachable, knownTargets)
    assert(!extra.contains("//protos:foo_proto"))
  }

  test("extra-dependency-labels-excludes-known-targets") {
    val dump = new BazelTargetsXmlDump(xml)
    val knownTargets = Set("//app:app", "//protos:foo_java_proto")
    val reachable = dump.reachableLabels(knownTargets.toList)
    val extra = dump.extraDependencyLabels(reachable, knownTargets)
    assertEquals(
      extra,
      Set(
        "@com_google_protobuf//:protobuf_java",
        "@com_google_protobuf//java/core:core",
      ),
    )
  }

  test("get-labels-actual-resolves-alias-to-underlying-target") {
    val dump = new BazelTargetsXmlDump(xml)
    assertEquals(
      dump.getLabels("actual").get("@com_google_protobuf//:protobuf_java"),
      Some(List("@com_google_protobuf//java/core:core")),
    )
  }

  test("rule-outputs-finds-jar-on-plain-external-java-library") {
    val dump = new BazelTargetsXmlDump(xml)
    assertEquals(
      dump.ruleOutputsByTarget.get("@com_google_protobuf//java/core:core"),
      Some(
        List(
          "@com_google_protobuf//java/core:libcore.jar",
          "@com_google_protobuf//java/core:libcore-src.jar",
        )
      ),
    )
  }
}
