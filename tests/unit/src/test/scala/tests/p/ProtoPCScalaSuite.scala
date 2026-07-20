package tests.p

/**
 * Tests for Scala-to-proto code navigation.
 *
 * Unlike the Java presentation compiler, the Scala presentation compiler has
 * no built-in notion of turbine's in-memory compiled classes -- proto-
 * generated classes must be materialized as real `.class` files (see
 * [[scala.meta.internal.metals.mbt.ProtoGeneratedClassFiles]]) and added to
 * the Scala build target's classpath for these to resolve at all.
 */
class ProtoPCScalaSuite extends BaseProtoPCSuite("proto-pc-scala") {

  test("scala-imports-nested-proto-message") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{"a": {}}
           |/a/src/main/proto/nested.proto
           |syntax = "proto3";
           |package com.example.api;
           |option java_package = "com.example.api.jproto";
           |option java_multiple_files = true;
           |message Container {
           |  message Inner {
           |    string value = 1;
           |  }
           |  Inner inner = 1;
           |}
           |/a/src/main/scala/com/example/Handler.scala
           |package com.example
           |import com.example.api.jproto.Container
           |object Handler {
           |  def handle(container: Container): Container.Inner =
           |    container.getInner
           |}
           |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/proto/nested.proto")
      _ <- server.didOpen("a/src/main/scala/com/example/Handler.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Handler.scala")
      // Without the materialized turbine classes on the Scala target's
      // classpath, the whole `Container`/`Container.Inner` chain used to
      // resolve to `<X: error>`, surfacing as "not found" diagnostics here.
      _ = assertNoDiagnostics()
      // Navigate from Container import in Scala to message definition in nested.proto
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Handler.scala",
        "import com.example.api.jproto.Contai@@ner",
        """|a/src/main/proto/nested.proto:5:9: definition
           |message Container {
           |        ^^^^^^^^^
           |""".stripMargin,
      )
      // Navigate from the nested Container.Inner return type to nested.proto
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Handler.scala",
        "def handle(container: Container): Container.Inn@@er =",
        """|a/src/main/proto/nested.proto:6:11: definition
           |  message Inner {
           |          ^^^^^
           |""".stripMargin,
      )
    } yield ()
  }
}
