package tests.proto

import scala.meta.inputs.Input
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.proto.ProtoLayout

/**
 * The layout matrix: one `.proto` text, and every shape a generator could turn
 * it into.
 *
 * A suite written against a single layout passes while the others are broken, so
 * each option that moves a declaration between nesting levels gets its own row.
 */
class ProtoLayoutSuite extends munit.FunSuite {

  private def layoutOf(
      protoPath: String,
      protoFileContent: String,
      packagePrefix: String = "",
  ): ProtoLayout =
    ProtoLayout.fromInput(
      Input.VirtualFile(protoPath, protoFileContent),
      packagePrefix,
    )

  private val singleFile =
    """|syntax = "proto3";
       |package a.b;
       |option java_package = "x.y";
       |message Msg {
       |  string first_name = 1;
       |  message Inner { string value = 1; }
       |  enum Nested { UNSET = 0; }
       |}
       |enum Top { NONE = 0; }
       |service Svc {
       |  rpc Echo (Msg) returns (Msg);
       |}
       |""".stripMargin

  private val multipleFiles =
    s"""|syntax = "proto3";
        |package a.b;
        |option java_package = "x.y";
        |option java_multiple_files = true;
        |${singleFile.linesIterator.drop(3).mkString("\n")}
        |""".stripMargin

  // ---------------------------------------------------------------- packages

  test("configured-packages") {
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(layout.protoPackage, "a.b")
    assertEquals(layout.javaPackage, "x.y")
    // Both roots, because a Java symbol resolves under the second while
    // proto-to-proto navigation uses the first.
    assertEquals(layout.semanticdbPackages, Seq("a/b/", "x/y/"))
  }

  test("java-package-absent-roots-coincide") {
    val layout = layoutOf(
      "a/b/foo_bar.proto",
      """|syntax = "proto3";
         |package a.b;
         |message Msg {}
         |""".stripMargin,
    )
    assertEquals(layout.javaPackage, "a.b")
    assertEquals(layout.semanticdbPackages, Seq("a/b/"))
  }

  test("no-proto-package-declares-at-root") {
    val layout = layoutOf(
      "foo_bar.proto",
      """|syntax = "proto3";
         |message Msg {}
         |""".stripMargin,
    )
    assertEquals(layout.semanticdbPackages, Seq("_empty_/"))
    // Rooted at `_root_/`, matching the proto indexer: a package-less proto
    // declares `Msg#`, not `_empty_/Msg#`.
    assertEquals(layout.declarations.map(_.symbol), Seq("Msg#"))
  }

  test("package-prefix-adds-a-shaded-root") {
    val layout =
      layoutOf("a/b/foo_bar.proto", singleFile, packagePrefix = "grpc_shaded")
    assertEquals(
      layout.configuredPackages,
      Seq("a/b/", "x/y/", "grpc_shaded/x/y/", "grpc_shaded/a/b/"),
    )
    // The index is keyed by these, so a user setting must not shift them.
    assertEquals(layout.semanticdbPackages, Seq("a/b/", "x/y/"))
  }

  // ------------------------------------------------------------ outer class

  test("outer-class-derived-from-file-name") {
    assertEquals(
      layoutOf("a/b/foo_bar.proto", singleFile).outerClassName,
      "FooBar",
    )
  }

  test("outer-class-explicit-option-wins") {
    val layout = layoutOf(
      "a/b/foo_bar.proto",
      """|syntax = "proto3";
         |package a.b;
         |option java_outer_classname = "P";
         |message Msg {}
         |""".stripMargin,
    )
    assertEquals(layout.outerClassName, "P")
  }

  test("outer-class-collision-gets-suffix") {
    // protoc appends `OuterClass` rather than shadowing the declaration. Without
    // this the outline's outer class silently hides `message Company` and every
    // lookup lands on an empty class.
    val layout = layoutOf(
      "proto/company.proto",
      """|syntax = "proto3";
         |package a.b;
         |message Company { string name = 1; }
         |""".stripMargin,
    )
    assertEquals(layout.outerClassName, "CompanyOuterClass")
  }

  test("outer-class-collides-only-with-top-level-declarations") {
    // `Inner` is nested, so it cannot collide with the outer class.
    val layout = layoutOf(
      "proto/inner.proto",
      """|syntax = "proto3";
         |package a.b;
         |message Msg { message Inner {} }
         |""".stripMargin,
    )
    assertEquals(layout.outerClassName, "Inner")
  }

  test("outer-class-exists-under-java-multiple-files-too") {
    // It holds only static plumbing there, but it is still generated and still
    // file-name-derived, which is what lets a bare `x/y/FooBar#` be attributed.
    val layout = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assert(layout.javaMultipleFiles)
    assertEquals(layout.outerClassName, "FooBar")
    assert(layout.fileDerivedNames.contains("FooBar"))
  }

  // ----------------------------------------------------------- declarations

  test("declarations-include-nested-ones") {
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(
      layout.declarations.map(_.symbol).sorted,
      Seq(
        "a/b/Msg#", "a/b/Msg#Inner#", "a/b/Msg#Nested#", "a/b/Svc#", "a/b/Top#",
      ),
    )
    // Only top-level declarations get their own file; depth is not uniform.
    assertEquals(
      layout.topLevelDeclarations.map(_.name).sorted,
      Seq("Msg", "Svc", "Top"),
    )
  }

  test("top-level-symbols-of-a-message-file-include-its-or-builder") {
    // `Msg.java` declares `interface MsgOrBuilder` and `class Msg`, and the
    // accessors live on the interface. The path-derived name stays first --
    // consumers read the head of the list as the file's binary name.
    val layout = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(
      layout.topLevelSymbolsOf("x/y/", "Msg"),
      Seq("x/y/Msg#", "x/y/MsgOrBuilder#"),
    )
  }

  test("top-level-symbols-of-a-service-file-are-just-itself") {
    val layout = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(layout.topLevelSymbolsOf("x/y/", "Svc"), Seq("x/y/Svc#"))
  }

  test("top-level-symbols-in-the-single-file-layout-are-just-the-outer-class") {
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(layout.topLevelSymbolsOf("x/y/", "FooBar"), Seq("x/y/FooBar#"))
  }

  // --------------------------------------------------------------- matching

  private def matched(layout: ProtoLayout, symbol: String): String =
    layout
      .matches(Symbol(symbol))
      .map(found =>
        found.declaration.map(_.symbol).getOrElse(found.evidence.toString)
      )
      .getOrElse("<none>")

  test("matches-both-symbol-shapes-of-one-declaration") {
    // The same declaration under one option line: nested in the outer class, or
    // top-level. Both have to reach `message Msg`.
    val single = layoutOf("a/b/foo_bar.proto", singleFile)
    val multiple = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(matched(single, "x/y/FooBar#Msg#"), "a/b/Msg#")
    assertEquals(matched(multiple, "x/y/Msg#"), "a/b/Msg#")
  }

  test("matches-a-nested-message-under-either-layout") {
    val single = layoutOf("a/b/foo_bar.proto", singleFile)
    val multiple = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(matched(single, "x/y/FooBar#Msg#Inner#"), "a/b/Msg#Inner#")
    assertEquals(matched(multiple, "x/y/Msg#Inner#"), "a/b/Msg#Inner#")
  }

  test("matches-through-a-generator-suffix") {
    val multiple = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(matched(multiple, "x/y/MsgOrBuilder#"), "a/b/Msg#")
    assertEquals(matched(multiple, "x/y/Msg#Builder#"), "a/b/Msg#")
  }

  test("matches-a-service-through-every-stub-shape") {
    val multiple = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(matched(multiple, "x/y/Svc#"), "a/b/Svc#")
    assertEquals(matched(multiple, "x/y/SvcGrpc#"), "a/b/Svc#")
    assertEquals(matched(multiple, "x/y/SvcImplBase#"), "a/b/Svc#")
    assertEquals(matched(multiple, "x/y/SvcGrpc#SvcImplBase#"), "a/b/Svc#")
    assertEquals(matched(multiple, "x/y/SvcGrpc#SvcBlockingStub#"), "a/b/Svc#")
  }

  test("matches-the-deepest-declaration-first") {
    // A nested message beats its parent, and a member's owner beats an enclosing
    // outer class -- otherwise a shadowed name resolves to whichever declaration
    // came first in the file.
    val single = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(
      matched(single, "x/y/FooBar#Msg#Inner#getValue()."),
      "a/b/Msg#Inner#",
    )
    assertEquals(matched(single, "x/y/FooBar#Msg#getFirstName()."), "a/b/Msg#")
  }

  test("matches-a-scalapb-sub-package-layout") {
    // ScalaPB roots output in a sub-package named after the file, and gives each
    // message a companion object.
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(matched(layout, "x/y/foo_bar/Msg#"), "a/b/Msg#")
    assertEquals(matched(layout, "x/y/foo_bar/Msg."), "a/b/Msg#")
  }

  test("matches-a-file-derived-name-with-no-declaration") {
    // A bare outer class or ScalaPB file object names nothing the proto body
    // mentions, so only the file name can attribute it.
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    assertEquals(matched(layout, "x/y/FooBar#"), "FileDerivedName")
    assertEquals(matched(layout, "x/y/foo_bar/FooBarProto."), "FileDerivedName")
  }

  test("matches-under-the-configured-package-prefix") {
    val layout =
      layoutOf("a/b/foo_bar.proto", singleFile, packagePrefix = "grpc_shaded")
    assertEquals(matched(layout, "grpc_shaded/x/y/Msg#"), "a/b/Msg#")
  }

  test("invents-nothing") {
    val layout = layoutOf("a/b/foo_bar.proto", singleFile)
    // Right package, but a name the proto neither declares nor derives.
    assertEquals(matched(layout, "x/y/Unrelated#"), "<none>")
    // Right name, wrong package.
    assertEquals(matched(layout, "p/q/Msg#"), "<none>")
  }

  test("a-member-of-the-static-only-outer-class-has-no-declaration") {
    // Landing on the outer class and finding no message is the correct answer
    // for a class that genuinely holds none, not a broken outline.
    val layout = layoutOf("a/b/foo_bar.proto", multipleFiles)
    assertEquals(
      matched(layout, "x/y/FooBar#registerAllExtensions()."),
      "FileDerivedName",
    )
  }

  // --------------------------------------------------- chains & descriptors

  test("name-chain-after-package") {
    assertEquals(
      ProtoLayout.nameChainAfterPackage(Symbol("x/y/Foo#Bar#baz().")),
      List("Foo", "Bar", "baz"),
    )
    assertEquals(
      ProtoLayout.nameChainAfterPackage(Symbol("Foo#baz().")),
      List("Foo", "baz"),
    )
  }

  test("descriptors-after-package-keep-the-kind") {
    // A name chain cannot tell `baz.`, `baz().` and `baz#` apart, so anything
    // rebuilding a symbol has to go through the descriptors.
    assertEquals(
      ProtoLayout
        .descriptorsAfterPackage(Symbol("x/y/Foo#Bar#baz()."))
        .map(_.toString),
      List("Foo#", "Bar#", "baz()."),
    )
    assertEquals(
      ProtoLayout
        .descriptorsAfterPackage(Symbol("x/y/Foo#bar."))
        .map(_.toString),
      List("Foo#", "bar."),
    )
  }

  test("reroot-preserves-descriptor-kinds-and-disambiguators") {
    assertEquals(
      ProtoLayout.reroot(Symbol("a/b/Msg#name()."), "x/y/"),
      "x/y/Msg#name().",
    )
    // An overload disambiguator has to survive: `name().` and `name(+1).` are
    // different methods.
    assertEquals(
      ProtoLayout.reroot(Symbol("a/b/Msg#name(+1)."), "x/y/"),
      "x/y/Msg#name(+1).",
    )
    assertEquals(
      ProtoLayout.reroot(Symbol("a/b/Msg#value."), "x/y/"),
      "x/y/Msg#value.",
    )
    // A symbol with no package of its own, which is what javac reports for a
    // class it could not resolve.
    assertEquals(
      ProtoLayout.reroot(Symbol("Msg#name()."), "x/y/"),
      "x/y/Msg#name().",
    )
    // Re-rooting at the root package drops the package entirely.
    assertEquals(ProtoLayout.reroot(Symbol("a/b/Msg#"), "_root_/"), "Msg#")
  }

  test("nested-in-type-inserts-an-outer-class") {
    assertEquals(
      ProtoLayout.nestedInType(Symbol("x/y/Msg#name()."), "FooBar"),
      Some("x/y/FooBar#Msg#name()."),
    )
    assertEquals(
      ProtoLayout.nestedInType(Symbol("Msg#name()."), "FooBar"),
      Some("FooBar#Msg#name()."),
    )
    // Never nests the outer class inside itself.
    assertEquals(
      ProtoLayout.nestedInType(Symbol("x/y/FooBar#Msg#"), "FooBar"),
      None,
    )
  }

  test("chain-relations") {
    // A sub-package layout moves a level into the package, so one chain ending
    // with the other is as much agreement as there is.
    assert(
      ProtoLayout.nameChainsCorrespond(List("Model", "User"), List("User"))
    )
    assert(
      ProtoLayout.nameChainsCorrespond(List("User"), List("Model", "User"))
    )
    assert(!ProtoLayout.nameChainsCorrespond(List("User"), List("User", "get")))
    assert(!ProtoLayout.nameChainsCorrespond(List("User"), Nil))
    // The outer-class layout instead nests the query inside the declaration.
    assert(ProtoLayout.nameChainEncloses(List("Outer"), List("Outer", "Msg")))
    assert(!ProtoLayout.nameChainEncloses(List("Msg"), List("Outer", "Msg")))
    assert(!ProtoLayout.nameChainEncloses(Nil, List("Msg")))
  }

  test("package-symbol-of") {
    assertEquals(ProtoLayout.packageSymbolOf("x.y"), "x/y/")
    assertEquals(ProtoLayout.packageSymbolOf(""), "_root_/")
  }
}
