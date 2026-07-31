package tests.mbt

import java.nio.file.Files

import scala.meta.inputs.Input
import scala.meta.internal.jmbt.Mbt
import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.StringBloomFilter
import scala.meta.internal.metals.mbt.IndexedDocument
import scala.meta.internal.metals.mbt.ProtoEvidence
import scala.meta.internal.metals.mbt.ProtoJavaVirtualFile
import scala.meta.internal.metals.mbt.ProtoOutputMapping
import scala.meta.internal.metals.mbt.VirtualTextDocument
import scala.meta.internal.mtags.proto.ProtoLayout
import scala.meta.io.AbsolutePath
import scala.meta.pc

import munit.AnyFixture

/**
 * Which `.proto` a generated file or symbol came from, over a hand-built index.
 *
 * The mapping takes its index as injected functions, so the rules can be
 * exercised without running one -- which is the point: every row here is a layout
 * some generator actually produces, and a rule that special-cases one of them
 * passes its own test and fails the rest.
 */
class ProtoOutputMappingSuite extends munit.FunSuite {

  val workspace = new tests.TemporaryDirectoryFixture()
  override def munitFixtures: Seq[AnyFixture[_]] = List(workspace)

  private val singleFileProto =
    """|syntax = "proto3";
       |package a.b;
       |option java_package = "x.y";
       |message Msg {
       |  string first_name = 1;
       |  message Inner { string value = 1; }
       |}
       |service Svc {
       |  rpc Echo (Msg) returns (Msg);
       |}
       |""".stripMargin

  private val multipleFilesProto =
    """|syntax = "proto3";
       |package a.b;
       |option java_package = "x.y";
       |option java_multiple_files = true;
       |message Msg {
       |  string first_name = 1;
       |  message Inner { string value = 1; }
       |}
       |service Svc {
       |  rpc Echo (Msg) returns (Msg);
       |}
       |""".stripMargin

  /**
   * A hand-built index: files with the packages they declare, the symbols they
   * declare, and -- for `.proto` files -- their text, so a layout can be parsed
   * from it.
   */
  private class FakeIndex {
    private val protoTexts =
      collection.mutable.Map.empty[AbsolutePath, String]
    private val documents =
      collection.mutable.Map.empty[AbsolutePath, IndexedDocument]
    private val outlines =
      collection.mutable.Map.empty[AbsolutePath, Seq[VirtualTextDocument]]
    private var packagePrefix: String = ""

    def withPackagePrefix(prefix: String): FakeIndex = {
      packagePrefix = prefix
      this
    }

    def addProto(
        relativePath: String,
        text: String,
    ): AbsolutePath = {
      val file = write(relativePath, text)
      protoTexts(file) = text
      val layout = ProtoLayout.fromInput(
        Input.VirtualFile(file.toString(), text),
        packagePrefix,
      )
      documents(file) = document(
        file,
        Semanticdb.Language.PROTOBUF,
        layout.semanticdbPackages,
        layout.declarations.map(_.symbol),
      )
      file
    }

    /** A file a generator wrote to disk, which the index holds like any other. */
    def addGenerated(
        relativePath: String,
        packageSymbol: String,
        symbols: Seq[String],
        text: String = "",
    ): AbsolutePath = {
      val file = write(relativePath, text)
      val language =
        if (relativePath.endsWith(".scala")) Semanticdb.Language.SCALA
        else Semanticdb.Language.JAVA
      documents(file) = document(file, language, Seq(packageSymbol), symbols)
      file
    }

    /** An outline Metals synthesized, keyed by the proto it came from. */
    def addOutline(
        proto: AbsolutePath,
        className: String,
        packageSymbol: String,
        topLevelSymbols: Seq[String],
    ): Unit = {
      val outline = VirtualTextDocument(
        ProtoJavaVirtualFile.makeUri(proto, className),
        pc.Language.JAVA,
        s"package ${packageSymbol.stripSuffix("/").replace('/', '.')};",
        Seq(packageSymbol),
        topLevelSymbols,
      )
      outlines(proto) = outlines.getOrElse(proto, Seq.empty) :+ outline
    }

    def mapping: ProtoOutputMapping =
      new ProtoOutputMapping(
        workspace(),
        protoLayoutOf = file =>
          protoTexts
            .get(file)
            .map(text =>
              ProtoLayout.fromInput(
                Input.VirtualFile(file.toString(), text),
                packagePrefix,
              )
            ),
        filesInPackage = packageSymbol =>
          documents.iterator.collect {
            case (file, indexed)
                if indexed.semanticdbPackages.contains(packageSymbol) =>
              file
          },
        documentOf = documents.get,
        outlinesOf = proto => outlines.getOrElse(proto, Seq.empty),
        textOf = file => Option(file).filter(_.isFile).map(_.readText),
        allProtoFiles = () => protoTexts.keysIterator,
      )

    private def write(relativePath: String, text: String): AbsolutePath = {
      val file = workspace().resolve(relativePath)
      Files.createDirectories(file.toNIO.getParent)
      Files.write(file.toNIO, text.getBytes("UTF-8"))
      file
    }

    private def document(
        file: AbsolutePath,
        language: Semanticdb.Language,
        packages: Seq[String],
        symbols: Seq[String],
    ): IndexedDocument =
      IndexedDocument(
        file = file,
        oid = "",
        source = Mbt.IndexedDocument.Source.ON_DID_CHANGE_FILE,
        semanticdbPackages = packages,
        language = language,
        symbols = symbols.map(symbol =>
          Mbt.SymbolInformation.newBuilder().setSymbol(symbol).build()
        ),
        bloomFilter = StringBloomFilter.forEstimatedSize(256),
      )
  }

  // ------------------------------------------------------- the evidence ladder

  test("outline-path-evidence") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val virtual = AbsolutePath(
      ProtoJavaVirtualFile.makeUri(proto, "FooBar").getPath()
    )
    val origins = index.mapping.originsOfFile(virtual)
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.OutlinePath))
    assertEquals(origins.map(_.proto), Seq(proto))
  }

  test("materialized-outline-path-evidence") {
    // The file the client opens, as opposed to the virtual entry the compiler
    // resolves against. Both name the proto in their own path.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val materialized = workspace()
      .resolve(".metals")
      .resolve("readonly")
      .resolve("dependencies")
      .resolve("proto-generated")
      .resolve("proto")
      .resolve("foo_bar.proto")
      .resolve("FooBar.java")
    val origins = index.mapping.originsOfFile(materialized)
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.OutlinePath))
    assertEquals(origins.map(_.proto), Seq(proto))
    assert(index.mapping.isSynthesizedOutline(materialized.toURI.toString()))
  }

  test("banner-evidence-workspace-relative") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val generated = index.addGenerated(
      "target/gen/x/y/FooBar.java",
      "x/y/",
      Seq("x/y/FooBar#"),
      text = "// source: proto/foo_bar.proto\npackage x.y;\n",
    )
    val origins = index.mapping.originsOfFile(generated)
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.Banner))
    assertEquals(origins.map(_.proto), Seq(proto))
  }

  test("banner-evidence-relative-to-a-protoc-include-root") {
    // The path protoc writes is relative to its include root, not the workspace,
    // so resolving it against the workspace misses. Ending an indexed proto's
    // path with it is what actually identifies the file.
    val index = new FakeIndex
    val proto =
      index.addProto(
        "some/deep/include/root/a/b/foo_bar.proto",
        singleFileProto,
      )
    val generated = index.addGenerated(
      "target/gen/x/y/FooBar.java",
      "x/y/",
      Seq("x/y/FooBar#"),
      text = "// source: a/b/foo_bar.proto\npackage x.y;\n",
    )
    val origins = index.mapping.originsOfFile(generated)
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.Banner))
    assertEquals(origins.map(_.proto), Seq(proto))
  }

  test("banner-evidence-testing-only-header") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val generated = index.addGenerated(
      "target/gen/x/y/FooBar.java",
      "x/y/",
      Seq("x/y/FooBar#"),
      text = " // testing-only-source: proto/foo_bar.proto\npackage x.y;\n",
    )
    assertEquals(
      index.mapping.originsOfFile(generated).map(_.proto),
      Seq(proto),
    )
  }

  test("declared-type-evidence") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val origins = index.mapping.originsOfSymbol("x/y/Msg#")
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.DeclaredType))
    assertEquals(origins.map(_.proto), Seq(proto))
    assertEquals(origins.flatMap(_.declaration).map(_.symbol), Seq("a/b/Msg#"))
  }

  test("file-derived-name-evidence") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val origins = index.mapping.originsOfSymbol("x/y/FooBar#")
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.FileDerivedName))
    assertEquals(origins.map(_.proto), Seq(proto))
    assertEquals(origins.flatMap(_.declaration), Seq.empty)
  }

  test("declared-type-outranks-file-derived-name") {
    // Two protos in one package: `other.proto` declares nothing named `Msg`, so
    // only `foo_bar.proto` can be the origin -- but a bare `FooBar#` matches
    // `foo_bar.proto` by file name alone, and that must rank lower.
    val index = new FakeIndex
    val fooBar = index.addProto("proto/foo_bar.proto", singleFileProto)
    index.addProto(
      "proto/other.proto",
      """|syntax = "proto3";
         |package a.b;
         |option java_package = "x.y";
         |message Other {}
         |""".stripMargin,
    )
    val origins = index.mapping.originsOfSymbol("x/y/Msg#")
    assertEquals(origins.map(_.proto), Seq(fooBar))
    assertEquals(origins.map(_.evidence), Seq(ProtoEvidence.DeclaredType))
  }

  test("ambiguity-is-reported-not-guessed") {
    // Both protos declare a `Shared`, so both are offered and the caller shows
    // the alternatives instead of one being silently chosen.
    val index = new FakeIndex
    val declaration =
      """|syntax = "proto3";
         |package a.b;
         |option java_package = "x.y";
         |message Shared {}
         |""".stripMargin
    val first = index.addProto("proto/aaa.proto", declaration)
    val second = index.addProto("proto/bbb.proto", declaration)
    val origins = index.mapping.originsOfSymbol("x/y/Shared#")
    assertEquals(origins.map(_.proto), Seq(first, second))
    assert(origins.forall(_.evidence == ProtoEvidence.DeclaredType))
  }

  test("invents-no-origin") {
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", singleFileProto)
    // Under the configured package, but named after nothing the proto declares
    // or derives.
    assertEquals(index.mapping.originsOfSymbol("x/y/Unrelated#"), Seq.empty)
    // Under no configured package at all.
    assertEquals(index.mapping.originsOfSymbol("p/q/Msg#"), Seq.empty)
    // A package symbol is not something a generator declares.
    assertEquals(index.mapping.originsOfSymbol("x/y/"), Seq.empty)
    assertEquals(index.mapping.originsOfSymbol(""), Seq.empty)
  }

  test("a-generated-file-declaring-nothing-of-the-protos-has-no-origin") {
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", singleFileProto)
    val unrelated = index.addGenerated(
      "target/gen/x/y/Handwritten.java",
      "x/y/",
      Seq("x/y/Handwritten#"),
      text = "package x.y;\n",
    )
    assertEquals(index.mapping.originsOfFile(unrelated), Seq.empty)
  }

  // -------------------------------------------------------------- both layouts

  test("both-symbol-shapes-reach-one-declaration") {
    val single = new FakeIndex
    val singleProto = single.addProto("proto/foo_bar.proto", singleFileProto)
    val multiple = new FakeIndex
    val multipleProto =
      multiple.addProto("proto/foo_bar.proto", multipleFilesProto)

    val fromOuterClass = single.mapping.originsOfSymbol("x/y/FooBar#Msg#")
    val fromTopLevel = multiple.mapping.originsOfSymbol("x/y/Msg#")

    assertEquals(fromOuterClass.map(_.proto), Seq(singleProto))
    assertEquals(fromTopLevel.map(_.proto), Seq(multipleProto))
    assertEquals(
      fromOuterClass.flatMap(_.declaration).map(_.symbol),
      fromTopLevel.flatMap(_.declaration).map(_.symbol),
    )
  }

  test("or-builder-resolves-under-java-multiple-files") {
    // Accessors are declared on the interface, so a symbol reported against it
    // has to reach the message.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", multipleFilesProto)
    val origins = index.mapping.originsOfSymbol("x/y/MsgOrBuilder#")
    assertEquals(origins.map(_.proto), Seq(proto))
    assertEquals(origins.flatMap(_.declaration).map(_.symbol), Seq("a/b/Msg#"))
  }

  test("a-nested-message-stays-nested-under-java-multiple-files") {
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", multipleFilesProto)
    assertEquals(
      index.mapping
        .originsOfSymbol("x/y/Msg#Inner#")
        .flatMap(_.declaration)
        .map(_.symbol),
      Seq("a/b/Msg#Inner#"),
    )
  }

  test("a-service-resolves-from-every-stub-shape") {
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", multipleFilesProto)
    val shapes = Seq(
      "x/y/Svc#",
      "x/y/SvcImplBase#",
      "x/y/SvcGrpc#SvcImplBase#",
      "x/y/SvcGrpc#SvcBlockingStub#",
    )
    for (shape <- shapes)
      assertEquals(
        index.mapping
          .originsOfSymbol(shape)
          .flatMap(_.declaration)
          .map(_.symbol),
        Seq("a/b/Svc#"),
        s"expected $shape to reach the service",
      )
  }

  // ---------------------------------------------------------- declaring output

  test("outlines-declaring-picks-only-the-declaring-file") {
    // `java_multiple_files` gives one outline per top-level declaration, so
    // taking any of them would as often land on a sibling.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", multipleFilesProto)
    index.addOutline(proto, "Msg", "x/y/", Seq("x/y/Msg#", "x/y/MsgOrBuilder#"))
    index.addOutline(proto, "Svc", "x/y/", Seq("x/y/Svc#"))
    index.addOutline(proto, "FooBar", "x/y/", Seq("x/y/FooBar#"))

    assertEquals(
      index.mapping
        .outlinesDeclaring(proto, "x/y/Msg#first_name().")
        .map(_.binaryName()),
      Seq("x.y.Msg"),
    )
    assertEquals(
      index.mapping.outlinesDeclaring(proto, "x/y/Svc#").map(_.binaryName()),
      Seq("x.y.Svc"),
    )
    // The accessor interface is recorded by the message's own file, so a symbol
    // against it lands there rather than nowhere.
    assertEquals(
      index.mapping
        .outlinesDeclaring(proto, "x/y/MsgOrBuilder#")
        .map(_.binaryName()),
      Seq("x.y.Msg"),
    )
  }

  test("no-outer-class-fallback-under-java-multiple-files") {
    // The outer class holds no declarations there, so answering with it would
    // hand back an empty class.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", multipleFilesProto)
    index.addOutline(proto, "FooBar", "x/y/", Seq("x/y/FooBar#"))
    assertEquals(
      index.mapping.outlinesDeclaring(proto, "x/y/Absent#"),
      Seq.empty,
    )
  }

  test("outer-class-fallback-in-the-single-file-layout") {
    // There everything lives in that one file, so it is the answer.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    index.addOutline(proto, "FooBar", "x/y/", Seq("x/y/FooBar#"))
    assertEquals(
      index.mapping
        .outlinesDeclaring(proto, "x/y/Msg#")
        .map(_.binaryName()),
      Seq("x.y.FooBar"),
    )
  }

  test("generated-sources-declaring-the-symbol-exactly-come-first") {
    // Two generators, one message: protoc spells it `x/y/Msg#`, a generator
    // rooting its output in a per-file sub-package spells it `x/y/foo_bar/Msg#`.
    // Both are offered -- the one declaring the symbol as asked for goes first.
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", multipleFilesProto)
    val subPackaged = index.addGenerated(
      "target/src_managed/x/y/foo_bar/Msg.scala",
      "x/y/foo_bar/",
      Seq("x/y/foo_bar/Msg#"),
    )
    val exact =
      index.addGenerated("target/gen/x/y/Msg.java", "x/y/", Seq("x/y/Msg#"))

    val origins = index.mapping.originsOfSymbol("x/y/Msg#")
    assertEquals(origins.size, 1)
    val outputs = origins.head.generatedSources
    assertEquals(outputs.map(_.file), Seq(exact, subPackaged))
    assertEquals(outputs.map(_.isExactSymbol), Seq(true, false))
  }

  test("a-sibling-type-is-not-offered-as-the-declaring-output") {
    // Real protoc puts `Msg` and `MsgOrBuilder` in one file, but a generator that
    // splits them must not have `MsgOrBuilder.java` answer for `Msg#` -- landing
    // on a sibling is worse than landing nowhere.
    val index = new FakeIndex
    index.addProto("proto/foo_bar.proto", multipleFilesProto)
    index.addGenerated(
      "target/gen/x/y/MsgOrBuilder.java",
      "x/y/",
      Seq("x/y/MsgOrBuilder#"),
    )
    assertEquals(
      index.mapping.originsOfSymbol("x/y/Msg#").flatMap(_.generatedSources),
      Seq.empty,
    )
  }

  // ------------------------------------------------------------ outputsOfProto

  test("outputs-of-proto-finds-the-configured-package") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", multipleFilesProto)
    val message =
      index.addGenerated("target/gen/x/y/Msg.java", "x/y/", Seq("x/y/Msg#"))
    val service =
      index.addGenerated("target/gen/x/y/Svc.java", "x/y/", Seq("x/y/Svc#"))
    index.addGenerated("target/gen/p/q/Other.java", "p/q/", Seq("p/q/Other#"))
    assertEquals(
      index.mapping.outputsOfProto(proto).sortBy(_.toString()),
      Seq(message, service).sortBy(_.toString()),
    )
  }

  test("outputs-of-proto-finds-a-file-derived-sub-package") {
    // ScalaPB roots its output in a sub-package named after the file.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val generated = index.addGenerated(
      "target/src_managed/x/y/foo_bar/Msg.scala",
      "x/y/foo_bar/",
      Seq("x/y/foo_bar/Msg#", "x/y/foo_bar/Msg."),
    )
    assertEquals(index.mapping.outputsOfProto(proto), Seq(generated))
  }

  test("outputs-of-proto-sees-only-indexed-files") {
    // Output that was cleaned away or lives in a jar yields nothing, which is
    // not the same as there being no mapping.
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    assertEquals(index.mapping.outputsOfProto(proto), Seq.empty)
  }

  test("outputs-of-proto-never-answers-with-the-proto-itself") {
    // A proto document is registered under its `java_package` too, so a
    // package-keyed lookup returns it.
    val index = new FakeIndex
    val proto = index.addProto(
      "proto/foo_bar.proto",
      """|syntax = "proto3";
         |package a.b;
         |message Msg {}
         |""".stripMargin,
    )
    assertEquals(index.mapping.outputsOfProto(proto), Seq.empty)
    assertEquals(
      index.mapping.originsOfSymbol("a/b/Msg#").flatMap(_.generatedSources),
      Seq.empty,
    )
    // The proto is still found as the origin -- only as the origin.
    assertEquals(
      index.mapping.originsOfSymbol("a/b/Msg#").map(_.proto),
      Seq(proto),
    )
  }

  // ------------------------------------------------------------ package prefix

  test("resolves-under-a-shaded-package-prefix") {
    val index = new FakeIndex().withPackagePrefix("grpc_shaded")
    val proto = index.addProto("proto/foo_bar.proto", multipleFilesProto)
    assertEquals(
      index.mapping.originsOfSymbol("grpc_shaded/x/y/Msg#").map(_.proto),
      Seq(proto),
    )
  }

  // -------------------------------------------------------- synthesized or not

  test("is-synthesized-outline") {
    val index = new FakeIndex
    val proto = index.addProto("proto/foo_bar.proto", singleFileProto)
    val mapping = index.mapping
    assert(
      mapping.isSynthesizedOutline(
        ProtoJavaVirtualFile.makeUri(proto, "FooBar").toString
      )
    )
    assert(
      !mapping.isSynthesizedOutline(
        workspace().resolve("target/gen/x/y/FooBar.java").toURI.toString()
      )
    )
    assert(!mapping.isSynthesizedOutline(proto.toURI.toString()))
  }
}
