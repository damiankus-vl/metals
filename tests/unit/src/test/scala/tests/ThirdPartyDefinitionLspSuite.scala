package tests

import java.io.BufferedOutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry

import scala.concurrent.Future

import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.MessageActionItem
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes
import org.objectweb.asm.{ClassWriter => AsmClassWriter}

class ThirdPartyDefinitionLspSuite
    extends BaseRangesSuite("third-party-definition") {

  /**
   * Publishes a tiny synthetic "third-party" dependency (a fabricated
   * `com.example:testlib:1.0.0`, laid out as a real Maven repository under a
   * temp directory) instead of fetching a real published library, so these
   * tests exercise genuine third-party dependency navigation -- decompile,
   * materialize, second-hop navigation, Turbine classpath fallback -- without
   * depending on any actual open-source project. Returns the repo root's
   * `file:` URI, suitable for a `metals.json` `repositories` entry.
   */
  private def publishTestLibrary(): String = {
    def defaultConstructor(cw: AsmClassWriter): Unit = {
      val ctor =
        cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
      ctor.visitCode()
      ctor.visitVarInsn(Opcodes.ALOAD, 0)
      ctor.visitMethodInsn(
        Opcodes.INVOKESPECIAL,
        "java/lang/Object",
        "<init>",
        "()V",
        false,
      )
      ctor.visitInsn(Opcodes.RETURN)
      ctor.visitMaxs(1, 1)
      ctor.visitEnd()
    }

    val optionBytes = {
      val cw = new AsmClassWriter(0)
      cw.visit(
        Opcodes.V1_8,
        Opcodes.ACC_PUBLIC + Opcodes.ACC_ANNOTATION + Opcodes.ACC_INTERFACE + Opcodes.ACC_ABSTRACT,
        "com/example/lib/Option",
        null,
        "java/lang/Object",
        Array("java/lang/annotation/Annotation"),
      )
      List("name", "usage").foreach { element =>
        val mv = cw.visitMethod(
          Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT,
          element,
          "()Ljava/lang/String;",
          null,
          null,
        )
        mv.visitEnd()
      }
      cw.visitEnd()
      cw.toByteArray
    }

    val cmdLineParserBytes = {
      val cw = new AsmClassWriter(0)
      cw.visit(
        Opcodes.V1_8,
        Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
        "com/example/lib/CmdLineParser",
        null,
        "java/lang/Object",
        null,
      )
      defaultConstructor(cw)
      val parse =
        cw.visitMethod(Opcodes.ACC_PUBLIC, "parse", "()V", null, null)
      parse.visitCode()
      parse.visitInsn(Opcodes.RETURN)
      parse.visitMaxs(0, 1)
      parse.visitEnd()
      cw.visitEnd()
      cw.toByteArray
    }

    val starterBytes = {
      val cw = new AsmClassWriter(0)
      cw.visit(
        Opcodes.V1_8,
        Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
        "com/example/lib/Starter",
        null,
        "java/lang/Object",
        null,
      )
      val field = cw.visitField(
        Opcodes.ACC_PRIVATE,
        "m",
        "Ljava/lang/reflect/Method;",
        null,
        null,
      )
      field.visitEnd()
      defaultConstructor(cw)
      val mv: MethodVisitor = cw.visitMethod(
        Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
        "main",
        "([Ljava/lang/String;)V",
        null,
        null,
      )
      mv.visitCode()
      val start = new Label()
      mv.visitLabel(start)
      mv.visitInsn(Opcodes.ACONST_NULL)
      mv.visitVarInsn(Opcodes.ASTORE, 1)
      mv.visitVarInsn(Opcodes.ALOAD, 1)
      mv.visitMethodInsn(
        Opcodes.INVOKEVIRTUAL,
        "com/example/lib/CmdLineParser",
        "parse",
        "()V",
        false,
      )
      val end = new Label()
      mv.visitLabel(end)
      mv.visitInsn(Opcodes.RETURN)
      mv.visitLocalVariable(
        "args",
        "[Ljava/lang/String;",
        null,
        start,
        end,
        0,
      )
      mv.visitLocalVariable(
        "parser",
        "Lcom/example/lib/CmdLineParser;",
        null,
        start,
        end,
        1,
      )
      mv.visitMaxs(1, 2)
      mv.visitEnd()
      cw.visitEnd()
      cw.toByteArray
    }

    val repoRoot =
      AbsolutePath(Files.createTempDirectory("testlib-maven-repo"))
    val artifactDir = repoRoot
      .resolve("com")
      .resolve("example")
      .resolve("testlib")
      .resolve("1.0.0")
    Files.createDirectories(artifactDir.toNIO)

    Files.write(
      artifactDir.resolve("testlib-1.0.0.pom").toNIO,
      ("""|<project xmlns="http://maven.apache.org/POM/4.0.0">
          |  <modelVersion>4.0.0</modelVersion>
          |  <groupId>com.example</groupId>
          |  <artifactId>testlib</artifactId>
          |  <version>1.0.0</version>
          |  <packaging>jar</packaging>
          |</project>
          |""".stripMargin).getBytes(StandardCharsets.UTF_8),
    )

    val out = new JarOutputStream(
      new BufferedOutputStream(
        Files.newOutputStream(artifactDir.resolve("testlib-1.0.0.jar").toNIO)
      )
    )
    try {
      List(
        "com/example/lib/Option.class" -> optionBytes,
        "com/example/lib/CmdLineParser.class" -> cmdLineParserBytes,
        "com/example/lib/Starter.class" -> starterBytes,
      ).foreach { case (entryName, bytes) =>
        out.putNextEntry(new ZipEntry(entryName))
        out.write(bytes)
        out.closeEntry()
      }
    } finally out.close()

    repoRoot.toNIO.toUri.toString
  }

  test("declining-consent-yields-no-decompiled-definition") {
    cleanWorkspace()
    // Decline consent with "Cancel"; returning None here would instead fall
    // through to the test client's default grant.
    client.showMessageRequestHandler = { params =>
      Option.when(
        params.getMessage().startsWith("Metals is about to decompile")
      )(new MessageActionItem("Cancel"))
    }
    val repo = publishTestLibrary()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "repositories": ["$repo"],
            |    "libraryDependencies": [
            |      "com.example:testlib:1.0.0"
            |    ],
            |    "skipSources": true
            |  }
            |}
            |/a/src/main/scala/a/Main.scala
            |package a
            |import com.example.lib.Starter
            |object Main {
            |  Starter.main(Array("--help"))
            |}
            |""".stripMargin
      )
      locations <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import com.example.lib.Sta@@rter
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
      |import com.example.lib.Option
      |object Main {
      |  @Opt@@ion(name="-r",usage="recursively run something")
      |  val option: Boolean = false
      |}
      |""".stripMargin,
    customMetalsJson = Some(
      s"""
         |{
         |  "a": {
         |    "repositories": ["${publishTestLibrary()}"],
         |    "libraryDependencies": [
         |      "com.example:testlib:1.0.0"
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
      |import com.example.lib.Sta@@rter
      |object Main {
      |  Starter.ma@@in(Array("--help"))
      |}
      |""".stripMargin,
    customMetalsJson = Some(
      s"""
         |{
         |  "a": {
         |    "repositories": ["${publishTestLibrary()}"],
         |    "libraryDependencies": [
         |      "com.example:testlib:1.0.0"
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
            "dependencies/decompiled/testlib-1.0.0.jar/com/example/lib"
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
    val repo = publishTestLibrary()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "repositories": ["$repo"],
            |    "libraryDependencies": [
            |      "com.example:testlib:1.0.0"
            |    ],
            |    "skipSources": true
            |  }
            |}
            |/a/src/main/scala/a/Main.scala
            |package a
            |import com.example.lib.Starter
            |object Main {
            |  Starter.main(Array("--help"))
            |}
            |""".stripMargin
      )
      firstHop <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import com.example.lib.Sta@@rter
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
              "dependencies/decompiled/testlib-1.0.0.jar/com/example/lib/Starter.java"
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
    val repo = publishTestLibrary()
    for {
      _ <- initialize(
        s"""|/metals.json
            |{
            |  "a": {
            |    "repositories": ["$repo"],
            |    "libraryDependencies": [
            |      "com.example:testlib:1.0.0"
            |    ],
            |    "skipSources": true
            |  }
            |}
            |/a/src/main/scala/a/Main.scala
            |package a
            |import com.example.lib.Starter
            |object Main {
            |  Starter.main(Array("--help"))
            |}
            |""".stripMargin
      )
      firstHop <- server.definition(
        "a/src/main/scala/a/Main.scala",
        """|package a
           |import com.example.lib.Sta@@rter
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
            "dependencies/decompiled/testlib-1.0.0.jar/com/example/lib/CmdLineParser.java"
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
