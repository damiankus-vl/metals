package tests.decompile

import java.nio.file.Files

import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PlatformFileIO
import scala.meta.internal.metals.decompile.DecompiledJavaFiles
import scala.meta.io.AbsolutePath

class DecompiledJavaFilesSuite extends munit.FunSuite {

  private val sep = java.io.File.separator

  private def emptyJar(name: String = "test.jar"): AbsolutePath = {
    val zip = AbsolutePath(
      Files.createTempDirectory("decompiled-java-files").resolve(name)
    )
    FileIO.withJarFileSystem(zip, create = true, close = true) { _ => () }
    zip
  }

  private def workspace(): AbsolutePath =
    AbsolutePath(Files.createTempDirectory("workspace"))

  test("materializes a jar entry's decompiled code keyed by jar and path") {
    val jar = emptyJar("args4j-2.37.jar")
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer"))
    val ws = workspace()
    val content = "package com.example;\npublic class Outer {\n}\n"

    val result = DecompiledJavaFiles.materialize(ws, pathClass, Nil, content)

    assert(result.isDefined, "expected a materialized file")
    val javaFile = result.get
    assert(
      javaFile.toString.endsWith(
        s"${sep}.metals${sep}readonly${sep}dependencies${sep}decompiled${sep}args4j-2.37.jar${sep}com${sep}example${sep}Outer.java"
      ),
      s"unexpected materialized path: $javaFile",
    )
    assertEquals(
      FileIO.slurp(javaFile, java.nio.charset.StandardCharsets.UTF_8),
      content,
    )
  }

  test("keys a nested class decompiled in isolation by its own binary name") {
    // NavigationTargetProvider decompiles the *enclosing* class as a whole
    // whenever it can, but the presentation compiler's own definition
    // resolution can still hand back an isolated nested class's `.class`
    // location directly; the materialized file must then be named after
    // that nested class (`Outer$Inner`), not the enclosing one, so it never
    // collides with (or is overwritten by) the enclosing class's own file.
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer$Inner"))
    val ws = workspace()

    val result =
      DecompiledJavaFiles.materialize(ws, pathClass, Nil, "class Outer.Inner")

    assert(result.isDefined)
    assert(
      result.get.toString.endsWith(s"Outer$$Inner.java"),
      s"expected the nested class's own name, got: ${result.get}",
    )
  }

  test(
    "a class-directory entry is keyed relative to its containing directory"
  ) {
    val classDir = AbsolutePath(Files.createTempDirectory("classdir"))
    val pathClass = classDir.resolve("com").resolve("example").resolve("Outer")
    val ws = workspace()

    val result = DecompiledJavaFiles.materialize(
      ws,
      pathClass,
      List(classDir),
      "package com.example;\npublic class Outer {}\n",
    )

    assert(result.isDefined)
    assert(
      result.get.toString.endsWith(
        s"decompiled${sep}workspace-classes${sep}com${sep}example${sep}Outer.java"
      ),
      s"unexpected materialized path: ${result.get}",
    )
  }

  test(
    "returns None when the class isn't inside a jar or any given directory"
  ) {
    val outsideDir = AbsolutePath(Files.createTempDirectory("outside"))
    val pathClass =
      outsideDir.resolve("com").resolve("example").resolve("Outer")
    val classDir = AbsolutePath(Files.createTempDirectory("classdir"))
    val ws = workspace()

    val result =
      DecompiledJavaFiles.materialize(ws, pathClass, List(classDir), "code")

    assertEquals(result, None)
  }

  test("re-materializing identical content does not rewrite the file") {
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer"))
    val ws = workspace()
    val content = "package com.example;\npublic class Outer {}\n"

    val first = DecompiledJavaFiles.materialize(ws, pathClass, Nil, content).get
    val firstModified = Files.getLastModifiedTime(first.toNIO)
    Thread.sleep(50)
    val second =
      DecompiledJavaFiles.materialize(ws, pathClass, Nil, content).get

    assertEquals(first, second)
    assertEquals(Files.getLastModifiedTime(second.toNIO), firstModified)
  }

  test("recovers the originating jar's filename from a materialized path") {
    val jar = emptyJar("args4j-2.37.jar")
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer"))
    val ws = workspace()

    val javaFile =
      DecompiledJavaFiles.materialize(ws, pathClass, Nil, "code").get

    assertEquals(
      DecompiledJavaFiles.jarFileNameOf(ws, javaFile),
      Some("args4j-2.37.jar"),
    )
  }

  test(
    "jarFileNameOf is None for a class-directory-sourced materialized file"
  ) {
    val classDir = AbsolutePath(Files.createTempDirectory("classdir"))
    val pathClass = classDir.resolve("com").resolve("example").resolve("Outer")
    val ws = workspace()

    val javaFile = DecompiledJavaFiles
      .materialize(ws, pathClass, List(classDir), "code")
      .get

    assertEquals(DecompiledJavaFiles.jarFileNameOf(ws, javaFile), None)
  }

  test("jarFileNameOf is None for a path outside the decompiled tree") {
    val ws = workspace()
    assertEquals(
      DecompiledJavaFiles.jarFileNameOf(ws, ws.resolve("Foo.java")),
      None,
    )
  }
}
