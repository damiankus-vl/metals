package tests.p

import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ServerCommands

/**
 * The Scala 3 counterpart of [[ProtoPCScalaSuite]].
 *
 * Scala 3 reaches the outlines through the same `listAllPackages` the Scala 2
 * compiler does. But its `ParsedLogicalPackage` lives in
 * `org.scala-lang:scala3-compiler_3`, and it resolves a source with
 * `AbstractFile.getFile`. That answers null for a path with no file on it. So a
 * Scala 3 target is served materialized outlines, not in-memory ones.
 */
class ProtoPCScala3Suite extends BaseProtoPCSuite("proto-pc-scala3") {

  // Generated code extends the protobuf runtime. Scala 3 typechecks the
  // outline, where Scala 2 only reads signatures out of it. So the target needs
  // the runtime on its classpath, as a real one would.
  private val protobufRuntime = "com.google.protobuf:protobuf-java:4.35.1"

  test("scala3-imports-nested-proto-message") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "scalaVersion": "${BuildInfo.scala3}",
            |    "libraryDependencies": ["$protobufRuntime"]
            |  }
            |}
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
      // With no file behind the outline paths, the
      // `Container`/`Container.Inner` chain fails to resolve here.
      _ = assertNoDiagnostics()
      // Unlike Scala 2, serving the compiler is what puts them on disk.
      _ = assert(
        workspace
          .resolve(
            ".metals/readonly/dependencies/proto-generated/a/src/main/proto/nested.proto/Container.java"
          )
          .exists,
        "expected the outline to be materialized for a Scala 3 target",
      )
      // Container import -> proto message definition
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Handler.scala",
        "import com.example.api.jproto.Contai@@ner",
        """|a/src/main/proto/nested.proto:5:9: definition
           |message Container {
           |        ^^^^^^^^^
           |""".stripMargin,
      )
      // Container.Inner return type -> nested proto message definition
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

  // Accessors come from the sibling `CompanyOrBuilder` interface that the same
  // outline declares. This resolves only once the file is read.
  test("scala3-navigates-from-generated-accessor") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "scalaVersion": "${BuildInfo.scala3}",
            |    "libraryDependencies": ["$protobufRuntime"]
            |  }
            |}
            |/a/src/main/proto/company.proto
            |syntax = "proto3";
            |package example.company;
            |option java_package = "com.example.proto";
            |option java_multiple_files = true;
            |option java_outer_classname = "CompanyProto";
            |message Company {
            |  string name = 1;
            |  repeated Department departments = 3;
            |  message Department {
            |    repeated Team teams = 3;
            |    message Team {
            |      string label = 1;
            |    }
            |  }
            |}
            |/a/src/main/scala/com/example/Summary.scala
            |package com.example
            |import com.example.proto.Company
            |object Summary {
            |  def f(company: Company): String =
            |    company.getName + company.getDepartments(0).getTeams(0).getLabel
            |}
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/com/example/Summary.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Summary.scala")
      _ = assertNoDiagnostics()
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Summary.scala",
        "company.getNa@@me + company",
        """|a/src/main/proto/company.proto:7:10: definition
           |  string name = 1;
           |         ^^^^
           |""".stripMargin,
      )
    } yield ()
  }

  // A Scala 3 target reads the outlines a compiler build wrote. A proto edit
  // is picked up only if that build is dropped. The signal for dropping it has
  // to come from somewhere both routes reach.
  test("scala3-picks-up-proto-change") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "scalaVersion": "${BuildInfo.scala3}",
            |    "libraryDependencies": ["$protobufRuntime"]
            |  }
            |}
            |/a/src/main/proto/model.proto
            |syntax = "proto3";
            |package com.example.api;
            |option java_package = "com.example.api.jproto";
            |option java_multiple_files = true;
            |message User {
            |  string name = 1;
            |}
            |/a/src/main/scala/com/example/Handler.scala
            |package com.example
            |import com.example.api.jproto.User
            |object Handler {
            |  def handle(user: User): String = user.getName
            |}
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/com/example/Handler.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Handler.scala")
      _ = assertNoDiagnostics()

      // Rename the message. The Scala file should stop compiling.
      _ <- server.didOpen("a/src/main/proto/model.proto")
      _ <- server.didChange("a/src/main/proto/model.proto") { _ =>
        """|syntax = "proto3";
           |package com.example.api;
           |option java_package = "com.example.api.jproto";
           |option java_multiple_files = true;
           |message Customer {
           |  string name = 1;
           |}
           |""".stripMargin
      }
      _ <- server.didSave("a/src/main/proto/model.proto")
      _ <- server.didFocus("a/src/main/scala/com/example/Handler.scala")

      _ = assert(
        client.workspaceDiagnostics.contains("User"),
        s"expected the stale `User` to stop resolving, got: ${client.workspaceDiagnostics}",
      )
    } yield ()
  }

  // Dotty picks between an outline and a classfile of the same name on their
  // timestamps. A materialized outline must not look newer than the build.
  test("scala3-outline-against-a-compiled-class-of-the-same-name") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "b": {},
            |  "a": {
            |    "dependsOn": ["b"],
            |    "scalaVersion": "${BuildInfo.scala3}",
            |    "libraryDependencies": ["$protobufRuntime"]
            |  }
            |}
            |/b/src/main/java/com/example/gen/Registry.java
            |package com.example.gen;
            |public class Registry {
            |  public static String fromCompiledClass() { return "compiled"; }
            |}
            |/a/src/main/proto/registry.proto
            |syntax = "proto3";
            |package com.example;
            |option java_package = "com.example.gen";
            |option java_outer_classname = "Registry";
            |message Entry {
            |  string key = 1;
            |}
            |/a/src/main/scala/com/example/Use.scala
            |package com.example
            |object Use {
            |  def compiled: String = com.example.gen.Registry.fromCompiledClass()
            |}
            |""".stripMargin
      )
      _ <- server.didOpen("b/src/main/java/com/example/gen/Registry.java")
      _ <- server.didFocus("b/src/main/java/com/example/gen/Registry.java")
      _ <- server.executeCommand(ServerCommands.CascadeCompile)
      _ <- server.didOpen("a/src/main/scala/com/example/Use.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Use.scala")
      _ = assertNoDiagnostics()
    } yield ()
  }
}
