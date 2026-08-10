package tests.p

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ServerCommands

import org.eclipse.{lsp4j => l}

/**
 * Tests for Scala-to-proto code navigation.
 *
 * The Java presentation compiler is handed the synthesized outlines through
 * its own file manager. The Scala one is handed them through
 * `SemanticdbFileManager#inMemorySourceFiles`.
 * `MbtWorkspaceSymbolProvider#protoJavaOutlineSourcePaths` puts their paths on
 * its source path. Drop either and proto-generated classes stop resolving.
 *
 * Nothing is written to serve those two. An outline reaches disk once
 * navigation sends the client to it. Scala 3 cannot be served that way. See
 * [[ProtoPCScala3Suite]].
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
      // Without the outlines on the source path the
      // `Container`/`Container.Inner` chain resolves to `<X: error>`. That
      // shows up as "not found" diagnostics here.
      _ = assertNoDiagnostics()
      // The chain resolved with no outline written. The compiler read them
      // from memory, the way the Java one does.
      _ = assert(
        !workspace
          .resolve(".metals/readonly/dependencies/proto-generated")
          .exists,
        "expected no outline to be materialized before navigating to one",
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
      // Navigating is what puts one on disk, so the client has a file to open.
      _ = assert(
        workspace
          .resolve(
            ".metals/readonly/dependencies/proto-generated/a/src/main/proto/nested.proto/Container.java"
          )
          .exists,
        "expected navigation to materialize the outline it lands in",
      )
    } yield ()
  }

  // Accessors are the case the Scala compiler reports with no source location.
  // Covered here on the message itself, and on one nested deeper, reached
  // through a chain of generated getters.
  test("scala-navigates-from-generated-accessor") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{"a": {}}
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
      // Accessors are inherited from the sibling `CompanyOrBuilder` interface
      // that the same outline declares.
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

  // The Scala compiler reads proto-generated classes through a source path
  // fixed when it was built. A proto edit has to regenerate the outlines and
  // rebuild the compiler.
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

      // The stale `User` class must no longer resolve.
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

  // A deleted proto never reaches `didSave`, so the save route cannot drop the
  // compilers that hold its outlines.
  test("scala-picks-up-proto-deletion") {
    cleanWorkspace()
    val proto = "a/src/main/proto/model.proto"
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

      _ = workspace.resolve(proto).delete()
      // Named by URI, since a relative name is resolved by looking the file up
      // on disk and it is gone by now.
      _ <- server.didChangeWatchedFiles(
        workspace.resolve(proto).toURI.toString(),
        l.FileChangeType.Deleted,
      )
      _ <- server.didFocus("a/src/main/scala/com/example/Handler.scala")

      // The classes the deleted proto generated must no longer resolve. The
      // whole `com.example.api.jproto` package goes with them, since the proto
      // was the only source of it.
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """|a/src/main/scala/com/example/Handler.scala:2:8: error: object api is not a member of package com.example
           |import com.example.api.jproto.User
           |       ^^^^^^^^^^^^^^^
           |a/src/main/scala/com/example/Handler.scala:4:20: error: not found: type User
           |  def handle(user: User): String = user.getName
           |                   ^^^^
           |""".stripMargin,
      )
    } yield ()
  }

  // Rebuilding the compilers is the expensive half of picking a proto change
  // up. A save that leaves the generated code alone skips it. The outlines
  // still have to survive that decision.
  test("scala-survives-a-save-that-changes-no-generated-code") {
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

      // A comment leaves the generated classes and accessors as they were.
      _ <- server.didOpen("a/src/main/proto/model.proto")
      _ <- server.didChange("a/src/main/proto/model.proto") { _ =>
        """|syntax = "proto3";
           |package com.example.api;
           |option java_package = "com.example.api.jproto";
           |option java_multiple_files = true;
           |// The person using the system.
           |message User {
           |  string name = 1;
           |}
           |""".stripMargin
      }
      _ <- server.didSave("a/src/main/proto/model.proto")
      _ <- server.didFocus("a/src/main/scala/com/example/Handler.scala")
      _ = assertNoDiagnostics()

      // The comment moved the message down a line. The proto is reparsed even
      // though no compiler was rebuilt.
      _ <- assertProtoDefinition(
        "a/src/main/scala/com/example/Handler.scala",
        "def handle(user: Us@@er)",
        """|a/src/main/proto/model.proto:6:9: definition
           |message User {
           |        ^^^^
           |""".stripMargin,
      )
    } yield ()
  }

  // A `.proto` can name a class a compiled dependency already provides, here
  // `com.example.gen.Registry`. The compiler keeps one of the two. It has to be
  // the classfile. Only that one has `fromCompiledClass` on it.
  test("scala-outline-against-a-compiled-class-of-the-same-name") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{
           |  "b": {},
           |  "a": {"dependsOn": ["b"]}
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
      // The suite does not build on open or focus. Without a build there is
      // no classfile for the outline to lose to.
      _ <- server.didOpen("b/src/main/java/com/example/gen/Registry.java")
      _ <- server.didFocus("b/src/main/java/com/example/gen/Registry.java")
      _ <- server.executeCommand(ServerCommands.CascadeCompile)
      _ <- server.didOpen("a/src/main/scala/com/example/Use.scala")
      _ <- server.didFocus("a/src/main/scala/com/example/Use.scala")
      _ = assertNoDiagnostics()
    } yield ()
  }
}
