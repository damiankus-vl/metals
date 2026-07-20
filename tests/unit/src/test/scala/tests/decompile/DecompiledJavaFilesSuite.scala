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

    val result = DecompiledJavaFiles.materialize(ws, pathClass, content)

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
    // Compilers.decompileAndLocate always redirects to the enclosing class
    // first, so this shouldn't happen in practice, but materialize itself
    // must still name the file after the nested class (`Outer$Inner`), not
    // the enclosing one, so it can't collide with that class's own file.
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer$Inner"))
    val ws = workspace()

    val result =
      DecompiledJavaFiles.materialize(ws, pathClass, "class Outer.Inner")

    assert(result.isDefined)
    assert(
      result.get.toString.endsWith(s"Outer$$Inner.java"),
      s"expected the nested class's own name, got: ${result.get}",
    )
  }

  test("returns None when the class isn't inside a jar") {
    val outsideDir = AbsolutePath(Files.createTempDirectory("outside"))
    val pathClass =
      outsideDir.resolve("com").resolve("example").resolve("Outer")
    val ws = workspace()

    val result = DecompiledJavaFiles.materialize(ws, pathClass, "code")

    assertEquals(result, None)
  }

  test("re-materializing identical content does not rewrite the file") {
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer"))
    val ws = workspace()
    val content = "package com.example;\npublic class Outer {}\n"

    val first = DecompiledJavaFiles.materialize(ws, pathClass, content).get
    val firstModified = Files.getLastModifiedTime(first.toNIO)
    Thread.sleep(50)
    val second =
      DecompiledJavaFiles.materialize(ws, pathClass, content).get

    assertEquals(first, second)
    assertEquals(Files.getLastModifiedTime(second.toNIO), firstModified)
  }

  test("recovers the originating jar's filename from a materialized path") {
    val jar = emptyJar("args4j-2.37.jar")
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val pathClass = AbsolutePath(fs.getPath("/com/example/Outer"))
    val ws = workspace()

    val javaFile =
      DecompiledJavaFiles.materialize(ws, pathClass, "code").get

    assertEquals(
      DecompiledJavaFiles.jarFileNameOf(ws, javaFile),
      Some("args4j-2.37.jar"),
    )
  }

  test("jarFileNameOf is None for a path outside the decompiled tree") {
    val ws = workspace()
    assertEquals(
      DecompiledJavaFiles.jarFileNameOf(ws, ws.resolve("Foo.java")),
      None,
    )
  }

  test("topLevelClassPath redirects a nested class to its enclosing class") {
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val nested = AbsolutePath(fs.getPath("/com/example/Outer$Inner"))

    assertEquals(
      DecompiledJavaFiles.topLevelClassPath(nested),
      AbsolutePath(fs.getPath("/com/example/Outer")),
    )
  }

  test(
    "topLevelClassPath redirects a doubly-nested class to its top-level enclosing class"
  ) {
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val nested = AbsolutePath(fs.getPath("/com/example/Outer$Middle$Inner"))

    assertEquals(
      DecompiledJavaFiles.topLevelClassPath(nested),
      AbsolutePath(fs.getPath("/com/example/Outer")),
    )
  }

  test("topLevelClassPath leaves a top-level class path unchanged") {
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val topLevel = AbsolutePath(fs.getPath("/com/example/Outer"))

    assertEquals(DecompiledJavaFiles.topLevelClassPath(topLevel), topLevel)
  }

  test(
    "topLevelClassPath leaves a top-level Scala object's own module class unchanged"
  ) {
    // A top-level module class (e.g. `scala.Some$`) has a trailing `$`, but
    // it's a module-class suffix, not a nesting separator -- it must not be
    // redirected to a same-named companion class, which may not even exist.
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val module = AbsolutePath(fs.getPath("/com/example/Some$"))

    assertEquals(DecompiledJavaFiles.topLevelClassPath(module), module)
  }

  test(
    "topLevelClassPath redirects a nested Scala object to its top-level enclosing class"
  ) {
    // `Outer$Inner$`: a nested object. The trailing `$` is a module-class
    // suffix, but the earlier `$` is still a real nesting separator that
    // must be found.
    val jar = emptyJar()
    val fs = PlatformFileIO.newJarFileSystem(jar, create = false)
    val nestedModule = AbsolutePath(fs.getPath("/com/example/Outer$Inner$"))

    assertEquals(
      DecompiledJavaFiles.topLevelClassPath(nestedModule),
      AbsolutePath(fs.getPath("/com/example/Outer")),
    )
  }

  test("topLevelClassPath also redirects a class-directory entry") {
    val classDir = AbsolutePath(Files.createTempDirectory("classdir"))
    val nested = classDir
      .resolve("com")
      .resolve("example")
      .resolve("Outer$Inner")

    assertEquals(
      DecompiledJavaFiles.topLevelClassPath(nested),
      classDir.resolve("com").resolve("example").resolve("Outer"),
    )
  }
}
