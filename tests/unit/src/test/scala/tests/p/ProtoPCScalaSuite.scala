package tests.p

import tests.BuildInfoVersions

/**
 * Tests for Scala-to-proto code navigation.
 *
 * Unlike the Java presentation compiler, the Scala one has no built-in
 * notion of turbine's in-memory compiled classes: proto-generated classes
 * must be materialized as real `.class` files (see
 * [[scala.meta.internal.metals.mbt.ProtoGeneratedClassFiles]]) and added to
 * the Scala target's classpath before they resolve at all.
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
      // Without the materialized turbine classes on the classpath, the whole
      // `Container`/`Container.Inner` chain used to resolve to `<X: error>`,
      // surfacing as "not found" diagnostics here.
      _ = assertNoDiagnostics()
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

  // Accessors are the case the Scala compiler reports with no source location
  // at all, on both the message itself and a deeply nested one reached through
  // a chain of generated getters.
  test("scala-navigates-from-generated-accessor") {
    cleanWorkspace()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "libraryDependencies": [
            |      "com.google.protobuf:protobuf-java:${BuildInfoVersions.protobufVersion}"
            |    ]
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
            |      repeated Employee employees = 3;
            |      message Employee {
            |        string full_name = 1;
            |      }
            |    }
            |  }
            |}
            |/a/src/main/scala/com/example/Summary.scala
            |package com.example
            |import com.example.proto.Company
            |object Summary {
            |  def f(company: Company): String = {
            |    val e = company.getDepartments(0).getTeams(0).getEmployees(0)
            |    company.getName + e.getFullName
            |  }
            |}
            |""".stripMargin
      )
      _ <- server.didOpen("a/src/main/scala/com/example/Summary.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Summary.scala")
      // Accessors are inherited from the sibling `CompanyOrBuilder`
      // interface, so they only resolve if it is on the classpath too.
      _ = assertNoDiagnostics()
      // Accessor on the top-level message -> the proto field it reads.
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Summary.scala",
        "company.getNa@@me + e.getFullName",
        """|a/src/main/proto/company.proto:7:10: definition
           |  string name = 1;
           |         ^^^^
           |""".stripMargin,
      )
      // Accessor reached through a chain of generated getters, on a message
      // nested three levels deep.
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Summary.scala",
        "getTeams(0).getEmploye@@es(0)",
        """|a/src/main/proto/company.proto:12:25: definition
           |      repeated Employee employees = 3;
           |                        ^^^^^^^^^
           |""".stripMargin,
      )
    } yield ()
  }

  // The Scala compiler reads proto-generated classes from a materialized
  // classpath directory fixed when the compiler was built, so a proto edit
  // has to both re-materialize the classes and rebuild the compiler.
  test("scala-picks-up-proto-change") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{"a": {}}
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

      // Rename the message in the proto; the Scala file should now fail.
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

      // The stale `User` class must no longer resolve off the materialized
      // classpath directory.
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """|a/src/main/scala/com/example/Handler.scala:2:31: error: object User is not a member of package com.example.api.jproto
           |import com.example.api.jproto.User
           |                              ^^^^
           |a/src/main/scala/com/example/Handler.scala:4:20: error: not found: type User
           |  def handle(user: User): String = user.getName
           |                   ^^^^
           |""".stripMargin,
      )
    } yield ()
  }
}
