package tests.decompile

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import scala.meta.dialects
import scala.meta.inputs.Input
import scala.meta.internal.metals.EmptyReportContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.decompile.DecompileBytecode
import scala.meta.internal.metals.decompile.DecompiledDeclarationSearch
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.{semanticdb => s}

import coursierapi.Dependency
import tests.BuildInfoVersions
import tests.Library

class DecompiledDeclarationSearchSuite extends munit.FunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  test("finds a class labeled `class Outer.Inner`") {
    // CFR's own label for a nested class decompiled without its enclosing
    // class alongside it -- not valid Java, but real, observed CFR output.
    val code =
      """|package com.example;
         |
         |public static final class Outer.Inner
         |extends Outer.Base {
         |    public Outer.Inner() {
         |    }
         |}
         |""".stripMargin

    val location = DecompiledDeclarationSearch.declarationLocation(
      code,
      "com/example/Outer#Inner#",
      "file:///Outer$Inner.java",
    )

    assert(location.isDefined, "expected a location to be found")
    val range = location.get.getRange
    assertEquals(range.getStart.getLine, 2)
    val declarationLine = code.split("\n", -1)(2)
    assertEquals(
      declarationLine.substring(
        range.getStart.getCharacter,
        range.getEnd.getCharacter,
      ),
      "Inner",
    )
  }

  test("returns None when no declaration matches the simple name") {
    val code = "package com.example;\npublic class Unrelated {\n}\n"
    val location = DecompiledDeclarationSearch.declarationLocation(
      code,
      "com/example/Outer#Inner#",
      "file:///Outer$Inner.java",
    )
    assertEquals(location, None)
  }

  test(
    "locates the real CFR output for a nested interface decompiled in isolation"
  ) {
    // Regression test for the bug this fallback fixes: go-to-definition on a
    // nested classpath type (e.g. `Descriptors.Descriptor` in protobuf-java)
    // landed on line 0 because CFR labels a nested type decompiled alone as
    // `interface Outer.Inner`, which mtags can't index as a real definition.
    // args4j is a small, already-used-elsewhere test dependency whose
    // `OptionHandlerRegistry.OptionHandlerFactory` reproduces the same shape:
    // a public static nested type inside a public top-level class.
    val jar = Library
      .fetch(Dependency.of("args4j", "args4j", "2.37"))
      .headOption
      .getOrElse(fail("could not resolve args4j:args4j:2.37"))

    val symbol =
      "org/kohsuke/args4j/OptionHandlerRegistry#OptionHandlerFactory#"
    val decompiled = Await.result(
      DecompileBytecode.cfr.decompile(
        "org.kohsuke.args4j.OptionHandlerRegistry$OptionHandlerFactory",
        List(jar),
      ),
      30.seconds,
    )
    val code = decompiled.getOrElse(fail(s"decompilation failed: $decompiled"))

    // Confirms the premise still holds for the CFR version in use; if CFR
    // changes this behavior, this fallback (and this test) can be removed.
    assert(
      code.contains("OptionHandlerRegistry.OptionHandlerFactory"),
      s"expected CFR's `Outer.Inner` labeling, got:\n$code",
    )

    val location = DecompiledDeclarationSearch.declarationLocation(
      code,
      symbol,
      "jar:file:///args4j.jar!/org/kohsuke/args4j/OptionHandlerRegistry$OptionHandlerFactory.class",
    )

    assert(
      location.isDefined,
      s"expected a location to be found in decompiled code:\n$code",
    )
    // It's important the line isn't 0, which would mean the fallback gave up
    // and returned the unchanged (0,0) location.
    assert(
      location.get.getRange.getStart.getLine > 0,
      s"expected a non-zero line, got: ${location.get}",
    )
  }

  test(
    "decompiling the whole enclosing class finds the nested declaration directly"
  ) {
    // NavigationTargetProvider resolves a nested type against its *enclosing*
    // class's own .class file (decompiled whole) rather than the nested
    // type's isolated one, precisely to avoid the `Outer.Inner` labeling
    // above: decompiled together with its enclosing class, CFR renders a
    // nested type as valid, correctly nested Java, so mtags' real javac-based
    // indexer finds the definition occurrence on its own -- no text-search
    // fallback needed.
    val jar = Library
      .fetch(Dependency.of("args4j", "args4j", "2.37"))
      .headOption
      .getOrElse(fail("could not resolve args4j:args4j:2.37"))

    val symbol =
      "org/kohsuke/args4j/OptionHandlerRegistry#OptionHandlerFactory#"
    val decompiled = Await.result(
      DecompileBytecode.cfr.decompile(
        "org.kohsuke.args4j.OptionHandlerRegistry",
        List(jar),
      ),
      30.seconds,
    )
    val code = decompiled.getOrElse(fail(s"decompilation failed: $decompiled"))

    implicit val rc: ReportContext = EmptyReportContext
    val doc = new Mtags().index(
      Input.VirtualFile("OptionHandlerRegistry.java", code),
      dialects.Scala213,
    )
    val occurrence = doc.occurrences.find(occ =>
      occ.role == s.SymbolOccurrence.Role.DEFINITION && occ.symbol == symbol
    )
    assert(
      occurrence.exists(_.range.exists(_.startLine > 0)),
      s"expected a definition occurrence for $symbol past line 0 in:\n$code",
    )
  }

  test("finds a Scala object's module class, named with a trailing `$`") {
    // A Scala object has no equivalent at the JVM level: it's compiled to a
    // separate module class whose binary name carries a trailing `$` (here
    // `Some$`), which is what CFR -- a Java decompiler with no notion of
    // Scala objects -- actually renders (`public final class Some$ {`).
    // Searching by the object symbol's simple name alone (`Some`) would never
    // match that declaration.
    val scalaLibrary =
      Library.getScalaLibraryJarPath(BuildInfoVersions.scala213)

    val decompiled = Await.result(
      DecompileBytecode.cfr.decompile("scala.Some$", List(scalaLibrary)),
      30.seconds,
    )
    val code = decompiled.getOrElse(fail(s"decompilation failed: $decompiled"))

    assert(
      code.contains("class Some$"),
      s"expected CFR's `$$`-suffixed module class name, got:\n$code",
    )

    val location = DecompiledDeclarationSearch.declarationLocation(
      code,
      "scala/Some.",
      "jar:file:///scala-library.jar!/scala/Some$.class",
    )

    assert(
      location.isDefined,
      s"expected a location to be found in decompiled code:\n$code",
    )
    assertEquals(
      location.get.getRange.getStart.getLine,
      code.linesIterator.indexWhere(_.contains("class Some$")),
    )
  }

  test("still finds an ordinary Scala case class (no trailing `$`)") {
    val scalaLibrary =
      Library.getScalaLibraryJarPath(BuildInfoVersions.scala213)

    val decompiled = Await.result(
      DecompileBytecode.cfr.decompile("scala.Some", List(scalaLibrary)),
      30.seconds,
    )
    val code = decompiled.getOrElse(fail(s"decompilation failed: $decompiled"))

    val location = DecompiledDeclarationSearch.declarationLocation(
      code,
      "scala/Some#",
      "jar:file:///scala-library.jar!/scala/Some.class",
    )

    assert(
      location.isDefined,
      s"expected a location to be found in decompiled code:\n$code",
    )
    assert(
      location.get.getRange.getStart.getLine > 0,
      s"expected a non-zero line, got: ${location.get}",
    )
  }
}
