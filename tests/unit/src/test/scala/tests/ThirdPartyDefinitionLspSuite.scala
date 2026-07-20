package tests

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.concurrent.Future

import org.eclipse.lsp4j.MessageActionItem

class ThirdPartyDefinitionLspSuite
    extends BaseRangesSuite("third-party-definition") {

  test("declining-consent-yields-no-decompiled-definition") {
    cleanWorkspace()
    // Decline consent with "Cancel"; returning None here would instead fall
    // through to the test client's default grant.
    client.showMessageRequestHandler = { params =>
      Option.when(
        params.getMessage().startsWith("Metals is about to decompile")
      )(new MessageActionItem("Cancel"))
    }
    for {
      _ <- initialize(
        """|/metals.json
           |{
           |  "a": {
           |    "libraryDependencies": [
           |      "args4j:args4j:2.37"
           |    ],
           |    "skipSources": true
           |  }
           |}
           |/a/src/main/scala/a/Main.scala
           |package a
           |import org.kohsuke.args4j.Starter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin
      )
      locations <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import org.kohsuke.args4j.Sta@@rter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin,
        workspace,
      )
    } yield assert(
      locations.isEmpty,
      s"Expected no definition when consent is declined, got: $locations",
    )
  }

  check(
    "definition-from-third-party-library",
    """
      |/a/src/main/scala/a/Main.scala
      |package a
      |import org.kohsuke.args4j.Option
      |object Main {
      |  @Opt@@ion(name="-r",usage="recursively run something")
      |  val option: Boolean = false
      |}
      |""".stripMargin,
    customMetalsJson = Some(
      """
        |{
        |  "a": {
        |    "libraryDependencies": [
        |      "args4j:args4j:2.37"
        |    ],
        |    "skipSources": true
        |  }
        |}
        |""".stripMargin
    ),
  )

  check(
    "call-static-method",
    """
      |/a/src/main/scala/a/Main.scala
      |package a
      |import org.kohsuke.args4j.Sta@@rter
      |object Main {
      |  Starter.ma@@in(Array("--help"))
      |}
      |""".stripMargin,
    customMetalsJson = Some(
      """
        |{
        |  "a": {
        |    "libraryDependencies": [
        |      "args4j:args4j:2.37"
        |    ],
        |    "skipSources": true
        |  }
        |}
        |""".stripMargin
    ),
  )

  override def assertCheck(
      filename: String,
      edit: String,
      expected: Map[String, String],
      base: Map[String, String],
  ): Future[Unit] = {
    for {
      locations <- server.definition(filename, edit, workspace)
    } yield {
      assert(locations.nonEmpty, s"Expected definition location but got none")
      val loc = locations(0)
      // Decompiled code is materialized to a real `.java` file, not left at a
      // `.class` URI (which no presentation compiler can open), so
      // goto-definition keeps working from inside it.
      assert(
        loc.getUri().endsWith(".java"),
        s"Expected a materialized .java definition location, instead got: ${loc}",
      )
      assert(
        loc
          .getUri()
          .contains(
            "dependencies/decompiled/args4j-2.37.jar/org/kohsuke/args4j"
          ),
        s"Expected definition location under the decompiled dependency tree, instead got: ${loc}",
      )
      // it's important the location is not 0,0, which would mean it wasn't found in decompiled code
      assert(
        loc.getRange().getStart().getLine() > 0,
        s"Expected definition location to start on a line greater than 0, instead got: ${loc}",
      )
    }
  }

  test("navigate-further-from-decompiled-code") {
    // Verify a second hop works from inside materialized decompiled code:
    // from Starter.java (reached by navigating from workspace source),
    // navigate to a type it references, java.lang.reflect.Method, using the
    // file's real decompiled content rather than a synthetic stand-in.
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{
           |  "a": {
           |    "libraryDependencies": [
           |      "args4j:args4j:2.37"
           |    ],
           |    "skipSources": true
           |  }
           |}
           |/a/src/main/scala/a/Main.scala
           |package a
           |import org.kohsuke.args4j.Starter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin
      )
      firstHop <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import org.kohsuke.args4j.Sta@@rter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin,
        workspace,
      )
      decompiledUri = {
        val loc = firstHop.headOption.getOrElse(
          fail("expected a first-hop definition location")
        )
        assert(
          loc
            .getUri()
            .endsWith(
              "dependencies/decompiled/args4j-2.37.jar/org/kohsuke/args4j/Starter.java"
            ),
          s"expected the materialized Starter.java, instead got: $loc",
        )
        loc.getUri()
      }
      queryWithMarker = {
        val realContent = new String(
          Files.readAllBytes(Paths.get(URI.create(decompiledUri))),
          StandardCharsets.UTF_8,
        )
        val marked = realContent.replaceFirst("Method m;", "@@Method m;")
        assert(
          marked != realContent,
          s"expected a 'Method m;' declaration in the real decompiled content:\n$realContent",
        )
        marked
      }
      secondHop <- server.definition(decompiledUri, queryWithMarker, workspace)
    } yield {
      val loc = secondHop.headOption.getOrElse(
        fail("expected goto-definition to work from inside decompiled code")
      )
      assert(
        loc.getUri().endsWith("java/lang/reflect/Method.java"),
        s"expected navigating further to java.lang.reflect.Method, instead got: $loc",
      )
      assert(
        loc.getRange().getStart().getLine() > 0,
        s"expected a non-zero line, instead got: $loc",
      )
    }
  }

  test("navigate-to-another-third-party-type-in-the-same-jar") {
    // [[TurbineClasspathFileManager]] skips binding a jar into a Java
    // target's classpath whenever that jar is also part of Turbine's
    // "global" classpath, assuming the global binder already covers it. That
    // only holds once a Turbine compile has actually bound it; outside
    // MBT/Bazel workspaces nothing triggers that compile, so the jar
    // resolves through neither classpath, and references to a second type in
    // the same jar (Starter -> CmdLineParser) used to silently fail. Verify
    // that navigation now succeeds.
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{
           |  "a": {
           |    "libraryDependencies": [
           |      "args4j:args4j:2.37"
           |    ],
           |    "skipSources": true
           |  }
           |}
           |/a/src/main/scala/a/Main.scala
           |package a
           |import org.kohsuke.args4j.Starter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin
      )
      firstHop <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import org.kohsuke.args4j.Sta@@rter
           |object Main {
           |  Starter.main(Array("--help"))
           |}
           |""".stripMargin,
        workspace,
      )
      decompiledUri = firstHop.headOption
        .getOrElse(fail("expected a first-hop definition location"))
        .getUri()
      queryWithMarker = {
        val realContent = new String(
          Files.readAllBytes(Paths.get(URI.create(decompiledUri))),
          StandardCharsets.UTF_8,
        )
        val marked = realContent.replaceFirst(
          "CmdLineParser parser = null;",
          "@@CmdLineParser parser = null;",
        )
        assert(
          marked != realContent,
          s"expected a 'CmdLineParser parser = null;' declaration in the real decompiled content:\n$realContent",
        )
        marked
      }
      secondHop <- server.definition(decompiledUri, queryWithMarker, workspace)
    } yield {
      val loc = secondHop.headOption.getOrElse(
        fail(
          "expected goto-definition to resolve another third-party type in the same jar"
        )
      )
      assert(
        loc
          .getUri()
          .endsWith(
            "dependencies/decompiled/args4j-2.37.jar/org/kohsuke/args4j/CmdLineParser.java"
          ),
        s"expected navigating further to the materialized CmdLineParser.java, instead got: $loc",
      )
      assert(
        loc.getRange().getStart().getLine() > 0,
        s"expected a non-zero line, instead got: $loc",
      )
    }
  }
}
