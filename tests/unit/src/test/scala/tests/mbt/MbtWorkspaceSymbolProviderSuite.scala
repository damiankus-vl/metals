package tests.mbt

import java.nio.file.Files
import java.nio.file.Paths

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

import scala.meta.internal.metals.Configs
import scala.meta.internal.metals.mbt.IndexingStats
import scala.meta.internal.metals.mbt.MbtWorkspaceSymbolProvider
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import munit.AnyFixture
import munit.TestOptions
import org.eclipse.{lsp4j => l}
import tests.CustomLoggingFixture
import tests.FileLayout
import tests.TemporaryDirectoryFixture

class MbtWorkspaceSymbolSearchSuite extends munit.FunSuite {
  case class Query(value: String, expected: String)
  val workspace = new TemporaryDirectoryFixture()
  override def munitFixtures: Seq[AnyFixture[_]] =
    List(
      workspace,
      CustomLoggingFixture.showWarnings(),
    )

  override def munitExecutionContext: ExecutionContext = ExecutionContext.global
  def formatSymbols(symbols: List[l.SymbolInformation]): String = {
    symbols
      .sortBy(s => s.getName() + s.getContainerName())
      .map(s => s"${s.getKind()} ${s.getName()} ${s.getContainerName()}")
      .mkString("\n")
  }
  def newProvider(): MbtWorkspaceSymbolProvider =
    new MbtWorkspaceSymbolProvider(
      workspace(),
      config = () => Configs.WorkspaceSymbolProviderConfig.mbt,
    )(munitExecutionContext)

  test("multi-language") {
    FileLayout.fromString(
      """
/com/Hello.scala
package com;
object Hello {
  def main(args: Array[String]): Unit = {
    println(Greeting.greet(User(name = "World", age = 20)))
  }
}
/com/Greeting.java
package com;
public class Greeting {
  enum Day { WORKDAY, WEEKEND }
  public static String greet(User user) {
    return "Hello, " + user.name + "!";
  }
}
/com/User.proto
package com;
message User {
  string name = 1;
  int32 age = 2;
}
/README.md
# Example Project
""",
      root = workspace(),
    )
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 3, updatedFiles = 3),
    )
    assertNoDiff(
      formatSymbols(provider.queryWorkspaceSymbol("Hel")),
      """
        |Object Hello com.
        |""".stripMargin,
    )
    assertNoDiff(
      formatSymbols(provider.queryWorkspaceSymbol("Greet")),
      """
        |Class Greeting com.
        |""".stripMargin,
    )
    val List(workday) = provider.queryWorkspaceSymbol("WORK")
    assert(clue(workday.getLocation().getRange().getStart().getLine()) > 0)
    assertNoDiff(
      formatSymbols(provider.queryWorkspaceSymbol("User")),
      """
        |Class User com.
        |""".stripMargin,
    )
    FileLayout.fromString(
      """
/com/Hello.scala
package com;
object Hello {
  def main(args: Array[String]): Unit = {
    println(Greeting.greet(User(name = "World", age = 20)))
  }
  def main2(): Unit = {
    println(Greeting.greet(User(name = "World", age = 20)))
  }
}
/com/Hello2.scala
package com;
object Hello2 {
  def main2(args: Array[String]): Unit = {
    println(Greeting.greet(User(name = "World", age = 20)))
  }
}
""",
      root = workspace(),
    )
    workspace.gitCommitAllChanges()
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 2),
    )
    assertNoDiff(
      formatSymbols(provider.queryWorkspaceSymbol("main")),
      """
        |Method main2 com.Hello.
        |Method main2 com.Hello2.
        |Method main com.Hello.
        |""".stripMargin,
    )
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 4, updatedFiles = 0),
    )

    // Remove a file
    Files.delete(workspace().resolve("com/Hello.scala").toNIO)
    workspace.gitCommitAllChanges()
    assertEquals(
      provider.onReindex().awaitBackgroundJobs(),
      IndexingStats(totalFiles = 3, updatedFiles = 0),
    )
    // Nothing to re-index, we only removed a file
    assertNoDiff(
      formatSymbols(provider.queryWorkspaceSymbol("main")),
      // No stale results from the deleted file
      """
        |Method main2 com.Hello2.
        |""".stripMargin,
    )
  }

  test("exclude-module-info-java") {
    FileLayout.fromString(
      """
/com/Hello.java
package com;
public class Hello {}
/module-info.java
module com.example {
  requires java.base;
  exports com;
}
""",
      root = workspace(),
    )
    val provider = newProvider()
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    val stats = provider.onReindex().awaitBackgroundJobs()
    assertEquals(stats, IndexingStats(totalFiles = 2, updatedFiles = 1))
    assertEquals(
      provider.allFiles().map(_.toRelative(workspace()).toString()),
      List(Paths.get("com/Hello.java").toString()),
    )
  }

  test("exclude-module-info-java-on-did-change") {
    FileLayout.fromString(
      """
/module-info.java
module com.example {
  requires java.base;
}
""",
      root = workspace(),
    )
    val provider = newProvider()
    Await.result(
      provider.onDidChange(workspace().resolve("module-info.java")),
      5.seconds,
    )
    assertEquals(provider.allFiles(), Nil)
  }

  // A Scala compiler holds the outlines it was built with. It records this
  // version first, and `Compilers` drops it once the version has moved on. A
  // save that generates the same code has to leave the version alone, or every
  // save on a `.proto` rebuilds the compilers.
  test("proto-outline-version") {
    def layout(message: String, comment: String): String =
      s"""|/com/User.proto
          |syntax = "proto3";
          |package com.example.api;
          |option java_package = "com.example.api.jproto";
          |option java_multiple_files = true;
          |$comment
          |message $message {
          |  string name = 1;
          |}
          |""".stripMargin
    FileLayout.fromString(layout("User", ""), root = workspace())
    val provider = indexedProvider()

    val proto = workspace().resolve("com/User.proto")
    assert(clue(provider.protoJavaOutlineSourcePaths()).nonEmpty)
    // Recording what a compiler was handed is its own step, taken where the
    // compiler is built.
    val served = provider.beginServingProtoOutlines()

    // A comment generates the same Java.
    FileLayout.fromString(layout("User", "// Someone."), root = workspace())
    assertEquals(provider.didSave(proto), false)
    assertEquals(provider.protoOutlineVersion(), served)

    // A renamed message does not.
    FileLayout.fromString(layout("Customer", ""), root = workspace())
    assertEquals(provider.didSave(proto), true)
    assertEquals(provider.protoOutlineVersion(), served + 1)
  }

  // Reading the source path is how a diagnostic names its candidate pool, and
  // that happens long after the compiler was built. Were the read to record
  // what it returns, the next save would compare the protos against themselves
  // and conclude the generated code had not changed.
  test("proto-outline-version-survives-a-source-path-read") {
    def layout(message: String): String =
      s"""|/com/User.proto
          |syntax = "proto3";
          |package com.example.api;
          |option java_package = "com.example.api.jproto";
          |option java_multiple_files = true;
          |message $message {
          |  string name = 1;
          |}
          |""".stripMargin
    FileLayout.fromString(layout("User"), root = workspace())
    val provider = indexedProvider()

    val proto = workspace().resolve("com/User.proto")
    val served = provider.beginServingProtoOutlines()

    // Edit the proto and let the index catch up, the way an editor buffer
    // change does. The compiler still holds the outlines for `User`.
    FileLayout.fromString(layout("Customer"), root = workspace())
    Await.result(provider.onDidChange(proto), 5.seconds)

    // Stands in for the diagnostics path, which reads the source path on every
    // compile error. It must not record `Customer` as what was served.
    assert(clue(provider.protoJavaOutlineSourcePaths()).nonEmpty)
    assert(clue(provider.protoJavaOutlineSourcePaths()).nonEmpty)

    assertEquals(provider.didSave(proto), true)
    assertEquals(provider.protoOutlineVersion(), served + 1)
  }

  // A proto declaring several toplevel messages generates one outline each,
  // and `java_package` sends them to a package the `.proto` does not name.
  // The lookup goes through the package index, which has to hold that Java
  // package as well as the proto one.
  private val multiMessageProto =
    """|/com/Model.proto
       |syntax = "proto3";
       |package com.example.api;
       |option java_package = "com.example.api.jproto";
       |option java_multiple_files = true;
       |message User {
       |  string name = 1;
       |}
       |message Order {
       |  string id = 1;
       |}
       |message Invoice {
       |  string number = 1;
       |}
       |""".stripMargin

  /**
   * A provider over whatever the workspace holds now, with proto indexing on.
   *
   * The index reads `git ls-files`, so a file written by [[FileLayout]] has to
   * be committed before it is visible. That is what the rest of this suite
   * does.
   */
  private def indexedProvider(
      protobufLspConfig: Configs.ProtobufLspConfig =
        Configs.ProtobufLspConfig.enabled
  ): MbtWorkspaceSymbolProvider = {
    val provider = new MbtWorkspaceSymbolProvider(
      workspace(),
      config = () => Configs.WorkspaceSymbolProviderConfig.mbt,
      protobufLspConfig = () => protobufLspConfig,
    )(munitExecutionContext)
    workspace.executeCommand("git init -b main")
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()
    provider
  }

  /**
   * What an outline declares, so one case reads as one expected literal.
   *
   * `packageSymbol` is the package the outline puts its classes in,
   * `com/example/api/jproto/`, which is the `java_package` rather than the
   * proto one. `toplevelSymbols` names the classes themselves,
   * `com/example/api/jproto/User#`.
   */
  private final case class DeclaringOutline(
      packageSymbol: String,
      toplevelSymbols: List[String],
  )

  private def declaringOutline(
      provider: MbtWorkspaceSymbolProvider,
      symbol: String,
  ): Option[DeclaringOutline] =
    provider
      .protoJavaOutlineFor(Symbol(symbol))
      .map(outline =>
        DeclaringOutline(
          packageSymbol = outline.pkg,
          toplevelSymbols = outline.toplevelSymbols().asScala.toList,
        )
      )

  test("proto-outline-lookup-across-several-toplevel-messages") {
    FileLayout.fromString(multiMessageProto, root = workspace())
    val provider = indexedProvider()

    // Each message resolves to its own outline, not to the first one the proto
    // generates. Every answer is written out. Building it from the message name
    // would agree with the lookup whatever the lookup came to return.
    assertEquals(
      declaringOutline(provider, "com/example/api/jproto/User#"),
      Some(
        DeclaringOutline(
          packageSymbol = "com/example/api/jproto/",
          toplevelSymbols = List("com/example/api/jproto/User#"),
        )
      ),
    )
    assertEquals(
      declaringOutline(provider, "com/example/api/jproto/Order#"),
      Some(
        DeclaringOutline(
          packageSymbol = "com/example/api/jproto/",
          toplevelSymbols = List("com/example/api/jproto/Order#"),
        )
      ),
    )
    assertEquals(
      declaringOutline(provider, "com/example/api/jproto/Invoice#"),
      Some(
        DeclaringOutline(
          packageSymbol = "com/example/api/jproto/",
          toplevelSymbols = List("com/example/api/jproto/Invoice#"),
        )
      ),
    )

    // A member is matched on its toplevel class.
    assertEquals(
      declaringOutline(provider, "com/example/api/jproto/Order#getId()."),
      Some(
        DeclaringOutline(
          packageSymbol = "com/example/api/jproto/",
          toplevelSymbols = List("com/example/api/jproto/Order#"),
        )
      ),
    )

    // The proto package generates nothing, so nothing answers under it.
    assertEquals(declaringOutline(provider, "com/example/api/User#"), None)
    // Neither does a symbol from an ordinary library.
    assertEquals(declaringOutline(provider, "scala/Option#"), None)
  }

  // `javaPackagePrefix` renames the protobuf runtime an outline refers to, for
  // a classpath that carries a shaded copy of it. It does not move the
  // generated class, so the package index still holds the plain
  // `java_package` and the lookup still resolves.
  test("proto-outline-lookup-with-a-shaded-protobuf-runtime") {
    FileLayout.fromString(multiMessageProto, root = workspace())
    val provider = indexedProvider(
      Configs.ProtobufLspConfig.enabled.copy(javaPackagePrefix = "grpc_shaded.")
    )

    assertEquals(
      declaringOutline(provider, "com/example/api/jproto/User#"),
      Some(
        DeclaringOutline(
          packageSymbol = "com/example/api/jproto/",
          toplevelSymbols = List("com/example/api/jproto/User#"),
        )
      ),
    )
    val outline = provider
      .protoJavaOutlineFor(Symbol("com/example/api/jproto/User#"))
      .getOrElse(fail("expected an outline under the unprefixed java_package"))

    // The prefix reaches the text, and only the text.
    assert(
      clue(outline.text).contains("grpc_shaded.com.google.protobuf."),
      "expected the outline to name the shaded runtime",
    )
    assert(
      !outline.text
        .replace("grpc_shaded.com.google.protobuf.", "")
        .contains("com.google.protobuf."),
      "expected no reference to the unshaded runtime",
    )
    assert(
      !outline.pkg.contains("grpc_shaded"),
      "expected the prefix to leave the generated class where it was",
    )
  }

  // A compiler holds the source path it was built with, so it cannot see a
  // proto indexed since. `didSave` cannot report this one: it runs before
  // indexing, where the new proto has no outlines to compare against.
  test("proto-outline-version-moves-on-a-newly-indexed-proto") {
    FileLayout.fromString(
      """|/com/User.proto
         |syntax = "proto3";
         |package com.example.api;
         |option java_package = "com.example.api.jproto";
         |option java_multiple_files = true;
         |message User {
         |  string name = 1;
         |}
         |""".stripMargin,
      root = workspace(),
    )
    val provider = indexedProvider()

    val served = provider.beginServingProtoOutlines()

    FileLayout.fromString(
      """|/com/Order.proto
         |syntax = "proto3";
         |package com.example.api;
         |option java_package = "com.example.api.jproto";
         |option java_multiple_files = true;
         |message Order {
         |  string id = 1;
         |}
         |""".stripMargin,
      root = workspace(),
    )
    // The index reads `git ls-files`, so the new proto has to be committed
    // before a reindex sees it.
    workspace.gitCommitAllChanges()
    provider.onReindex().awaitBackgroundJobs()

    assert(
      clue(provider.protoOutlineVersion()) > clue(served),
      "expected a proto indexed after the compiler was built to move the version",
    )
  }

  def manuallyTestWorkspace(
      dir: TestOptions,
      query: String,
      assertResultIncludes: String,
  ): Unit = {
    test(dir) {
      val provider = new MbtWorkspaceSymbolProvider(
        workspace = AbsolutePath(dir.name),
        config = () => Configs.WorkspaceSymbolProviderConfig.mbt,
      )(munitExecutionContext)
      provider.onReindex()
      val result =
        formatSymbols(provider.queryWorkspaceSymbol(query))
      scribe.info(
        result.split("\n").filter(l => l.startsWith("Class ")).mkString("\n")
      )
      assert(
        clue(result).contains(assertResultIncludes)
      )
    }
  }

  // Use this helper to manually test the indexer against a real-world codebase
  manuallyTestWorkspace(
    "/tmp/test-project".ignore,
    query = "TestProjectEnum",
    assertResultIncludes = "Object TestProjectEnum ",
  )

}
