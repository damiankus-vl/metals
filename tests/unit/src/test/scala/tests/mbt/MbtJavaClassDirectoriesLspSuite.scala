package tests.mbt

import java.io.BufferedOutputStream
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry

import scala.meta.internal.metals.AutoImportBuildKind
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.metals.mbt.MbtBuildServer
import scala.meta.io.AbsolutePath

import org.eclipse.lsp4j.Location
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes
import tests.BaseLspSuite
import tests.BuildInfo

/**
 * MBT (e.g. Bazel) targets don't run a real compile themselves, so
 * annotation-processor-generated classes (AutoValue, Lombok, ...) only exist
 * in the class output of a prior real build, declared via a namespace's
 * `classDirectories`. Checks that the Java presentation compiler resolves a
 * reference to such a compiled-only, source-less class instead of reporting
 * "cannot find symbol".
 */
class MbtJavaClassDirectoriesLspSuite
    extends BaseLspSuite("mbt-java-class-directories") {

  override def initializeGitRepo: Boolean = true

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      fallbackScalaVersion = Some(BuildInfo.scalaVersion),
      presentationCompilerDiagnostics = true,
      workspaceSymbolProvider = WorkspaceSymbolProviderConfig.mbt,
      preferredBuildServer = Some(MbtBuildServer.name),
      automaticImportBuild = AutoImportBuildKind.All,
    )

  private def classBytes(
      internalName: String,
      superInternalName: String,
      classFileVersion: Int,
  ): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(
      classFileVersion,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      internalName,
      null,
      superInternalName,
      null,
    )
    val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
    mv.visitCode()
    mv.visitVarInsn(Opcodes.ALOAD, 0)
    mv.visitMethodInsn(
      Opcodes.INVOKESPECIAL,
      superInternalName,
      "<init>",
      "()V",
      false,
    )
    mv.visitInsn(Opcodes.RETURN)
    mv.visitMaxs(1, 1)
    mv.visitEnd()
    cw.visitEnd()
    cw.toByteArray()
  }

  /**
   * Mimics a class jar an annotation processor's output ends up in after a
   * prior real build -- e.g. Bazel's `classDirectories` always point at a
   * `.jar`, never a bare directory.
   */
  private def writeCompiledOnlyClassJar(
      workspace: AbsolutePath,
      relativeJarPath: String,
      internalName: String,
      superInternalName: String,
      classFileVersion: Int = Opcodes.V1_8,
  ): Unit =
    writeCompiledOnlyClassesJar(
      workspace,
      relativeJarPath,
      List(internalName -> superInternalName),
      classFileVersion,
    )

  /**
   * Same as [[writeCompiledOnlyClassJar]], but for a jar with several
   * top-level classes -- e.g. AutoValue emits both `AutoValue_Foo` and a
   * `$`-prefixed abstract base `$AutoValue_Foo` for a `@Memoized`-annotated
   * value class, both top-level, with the former extending the latter.
   */
  private def writeCompiledOnlyClassesJar(
      workspace: AbsolutePath,
      relativeJarPath: String,
      classes: List[(String, String)],
      classFileVersion: Int = Opcodes.V1_8,
      precomputedBytes: Map[String, Array[Byte]] = Map.empty,
  ): Unit = {
    val jarFile = workspace.resolve(relativeJarPath)
    Files.createDirectories(jarFile.toNIO.getParent)
    val out = new JarOutputStream(
      new BufferedOutputStream(Files.newOutputStream(jarFile.toNIO))
    )
    try {
      classes.foreach { case (internalName, superInternalName) =>
        out.putNextEntry(new ZipEntry(s"$internalName.class"))
        out.write(
          precomputedBytes.getOrElse(
            internalName,
            classBytes(internalName, superInternalName, classFileVersion),
          )
        )
        out.closeEntry()
      }
    } finally out.close()
  }

  /**
   * Writes a jar with a top-level `outerInternalName` class that has a real
   * nested static class `outerInternalName$innerSimpleName` (an
   * `InnerClasses` attribute on both class files, mirroring what real javac
   * emits for e.g. `ClassInfo` and its nested `MemberInfo`), plus a third
   * top-level class `subclassInternalName` extending the nested class
   * directly -- mirroring AutoValue's
   * `$AutoValue_ClassInfo_MemberInfo extends ClassInfo.MemberInfo`.
   */
  private def writeNestedClassJar(
      workspace: AbsolutePath,
      relativeJarPath: String,
      outerInternalName: String,
      innerSimpleName: String,
      subclassInternalName: String,
  ): Unit = {
    val innerInternalName = s"$outerInternalName$$$innerSimpleName"

    def withDefaultConstructor(
        cw: ClassWriter,
        superInternalName: String,
    ): Unit = {
      val mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      mv.visitVarInsn(Opcodes.ALOAD, 0)
      mv.visitMethodInsn(
        Opcodes.INVOKESPECIAL,
        superInternalName,
        "<init>",
        "()V",
        false,
      )
      mv.visitInsn(Opcodes.RETURN)
      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }

    val outerWriter = new ClassWriter(0)
    outerWriter.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
      outerInternalName,
      null,
      "java/lang/Object",
      null,
    )
    outerWriter.visitInnerClass(
      innerInternalName,
      outerInternalName,
      innerSimpleName,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
    )
    withDefaultConstructor(outerWriter, "java/lang/Object")
    outerWriter.visitEnd()

    val innerWriter = new ClassWriter(0)
    innerWriter.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC + Opcodes.ACC_SUPER,
      innerInternalName,
      null,
      "java/lang/Object",
      null,
    )
    innerWriter.visitInnerClass(
      innerInternalName,
      outerInternalName,
      innerSimpleName,
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
    )
    withDefaultConstructor(innerWriter, "java/lang/Object")
    innerWriter.visitEnd()

    writeCompiledOnlyClassesJar(
      workspace,
      relativeJarPath,
      List(
        outerInternalName -> null,
        innerInternalName -> null,
        subclassInternalName -> innerInternalName,
      ),
      precomputedBytes = Map(
        outerInternalName -> outerWriter.toByteArray,
        innerInternalName -> innerWriter.toByteArray,
      ),
    )
  }

  test("autovalue-style-companion-class-resolves-from-class-directory") {
    for {
      _ <- initialize(
        """|/.metals/mbt.json
           |{
           |  "namespaces": {
           |    "a": {
           |      "sources": ["a/src/**"],
           |      "classDirectories": ["a/prebuilt-classes.jar"]
           |    }
           |  }
           |}
           |/a/src/example/Foo.java
           |package example;
           |
           |public abstract class Foo {
           |  public static Foo create() {
           |    AutoValue_Foo instance = new AutoValue_Foo();
           |    return instance;
           |  }
           |}
           |""".stripMargin,
        runAdditionalCommands = workspace =>
          writeCompiledOnlyClassJar(
            workspace,
            "a/prebuilt-classes.jar",
            "example/AutoValue_Foo",
            "example/Foo",
          ),
      )
      _ <- server.didOpen("a/src/example/Foo.java")
      _ <- server.didFocus("a/src/example/Foo.java")
      definitionLocations <- server.definitionSubstringQuery(
        "a/src/example/Foo.java",
        "AutoValue_@@Foo",
      )
    } yield {
      assertNoDiagnostics()
      assertOpenableDefinition(definitionLocations)
    }
  }

  test(
    "autovalue-style-companion-class-resolves-despite-newer-jdk-bytecode"
  ) {
    // Class-file major version 65 = JDK 21, newer than any JDK the Metals
    // server itself is expected to run on. Real javac's `-classpath` reader
    // would refuse to load this at all ("bad class file ... wrong version"),
    // which is exactly the failure this synthesized-outline mechanism
    // (feeding the class in via `-sourcepath` instead) is meant to avoid.
    val newerJdkClassFileVersion = 65
    for {
      _ <- initialize(
        """|/.metals/mbt.json
           |{
           |  "namespaces": {
           |    "a": {
           |      "sources": ["a/src/**"],
           |      "classDirectories": ["a/prebuilt-classes.jar"]
           |    }
           |  }
           |}
           |/a/src/example/Foo.java
           |package example;
           |
           |public abstract class Foo {
           |  public static Foo create() {
           |    AutoValue_Foo instance = new AutoValue_Foo();
           |    return instance;
           |  }
           |}
           |""".stripMargin,
        runAdditionalCommands = workspace =>
          writeCompiledOnlyClassJar(
            workspace,
            "a/prebuilt-classes.jar",
            "example/AutoValue_Foo",
            "example/Foo",
            classFileVersion = newerJdkClassFileVersion,
          ),
      )
      _ <- server.didOpen("a/src/example/Foo.java")
      _ <- server.didFocus("a/src/example/Foo.java")
      definitionLocations <- server.definitionSubstringQuery(
        "a/src/example/Foo.java",
        "AutoValue_@@Foo",
      )
    } yield {
      assertNoDiagnostics()
      assertOpenableDefinition(definitionLocations)
    }
  }

  test(
    "autovalue-memoized-style-dollar-prefixed-base-resolves-without-fallback"
  ) {
    // AutoValue emits an extra top-level, `$`-prefixed abstract base class
    // (not a nested class) for a `@Memoized`-annotated value class, e.g.
    // `$AutoValue_ClassInfo_MemberInfo`, which `AutoValue_ClassInfo_MemberInfo`
    // itself extends. If the outline provider's package listing wrongly
    // treats a leading `$` as a nested-class marker and skips it, the
    // `$`-prefixed class never gets its own synthesized outline -- so a
    // direct reference to IT (unlike a reference to its concrete subclass,
    // whose own definition node doesn't depend on its supertype resolving)
    // has no tree node anywhere on the virtual sourcepath, `compilerDefn`
    // comes up empty, and Metals silently falls back to decompiling the jar
    // through a completely separate mechanism -- this is the
    // double-materialization bug this test guards against.
    for {
      _ <- initialize(
        """|/.metals/mbt.json
           |{
           |  "namespaces": {
           |    "a": {
           |      "sources": ["a/src/**"],
           |      "classDirectories": ["a/prebuilt-classes.jar"]
           |    }
           |  }
           |}
           |/a/src/example/Foo.java
           |package example;
           |
           |public abstract class Foo {
           |  public static void create() {
           |    $AutoValue_ClassInfo_MemberInfo base =
           |        new AutoValue_ClassInfo_MemberInfo();
           |  }
           |}
           |""".stripMargin,
        runAdditionalCommands = workspace =>
          writeCompiledOnlyClassesJar(
            workspace,
            "a/prebuilt-classes.jar",
            List(
              "example/$AutoValue_ClassInfo_MemberInfo" -> "java/lang/Object",
              "example/AutoValue_ClassInfo_MemberInfo" ->
                "example/$AutoValue_ClassInfo_MemberInfo",
            ),
          ),
      )
      _ <- server.didOpen("a/src/example/Foo.java")
      _ <- server.didFocus("a/src/example/Foo.java")
      definitionLocations <- server.definitionSubstringQuery(
        "a/src/example/Foo.java",
        "$AutoValue_@@ClassInfo_MemberInfo",
      )
    } yield {
      assertNoDiagnostics()
      assertOpenableDefinition(definitionLocations)
      assertNotDecompiledAsFallback("$AutoValue_ClassInfo_MemberInfo")
    }
  }

  test(
    "goto-definition-into-nested-class-from-inside-compiled-only-outline"
  ) {
    // AutoValue's `$AutoValue_ClassInfo_MemberInfo` extends the real nested
    // class `ClassInfo.MemberInfo`, and CFR decompiles that reference as the
    // qualified `extends ClassInfo.MemberInfo` (confirmed against a real
    // Bazel workspace's decompiled output). Goto-definition on `MemberInfo`
    // from *inside* that decompiled/materialized outline file -- not from a
    // real workspace source -- must resolve to the nested class declared
    // inside `ClassInfo`'s own materialized outline. This requires
    // `Compilers.loadCompiler` to recognize the *current* file (the
    // materialized compiled-only outline) as belonging to this build target,
    // so its presentation compiler still has every sibling compiled-only
    // class synthesized on the `-sourcepath` -- not just the bare fallback
    // compiler, which resolves nothing else in the package.
    for {
      _ <- initialize(
        """|/.metals/mbt.json
           |{
           |  "namespaces": {
           |    "a": {
           |      "sources": ["a/src/**"],
           |      "classDirectories": ["a/prebuilt-classes.jar"]
           |    }
           |  }
           |}
           |/a/src/example/Foo.java
           |package example;
           |
           |public abstract class Foo {
           |  public static void create() {
           |    AutoValue_ClassInfo_MemberInfo instance =
           |        new AutoValue_ClassInfo_MemberInfo();
           |  }
           |}
           |""".stripMargin,
        runAdditionalCommands = workspace =>
          writeNestedClassJar(
            workspace,
            "a/prebuilt-classes.jar",
            outerInternalName = "example/ClassInfo",
            innerSimpleName = "MemberInfo",
            subclassInternalName = "example/AutoValue_ClassInfo_MemberInfo",
          ),
      )
      // Compiling Foo.java for diagnostics (not a definition query) already
      // forces the presentation compiler to read -- and thus materialize --
      // both outlines, as a side effect of resolving the type reference. A
      // `textDocument/definition` query is deliberately avoided here: a
      // *successful* one permanently records "this materialized destination
      // belongs to build target X" in `tables.dependencySources`
      // (`InteractiveSemanticdbs.didDefinition`), which would mask the very
      // routing gap this test targets on every subsequent query for the same
      // destination -- confirmed by tracing that exact cache during
      // development of this test.
      _ <- server.didOpen("a/src/example/Foo.java")
      _ <- server.didFocus("a/src/example/Foo.java")
      outlineUri = {
        assertNoDiagnostics()
        materializedCompiledOnlyOutlineUri("AutoValue_ClassInfo_MemberInfo")
      }
      _ <- server.didOpen(outlineUri)
      _ <- server.didFocus(outlineUri)
      nestedDefinitionLocations <- server.definitionSubstringQuery(
        outlineUri,
        "ClassInfo.@@MemberInfo",
      )
    } yield {
      assertOpenableDefinition(nestedDefinitionLocations)
    }
  }

  /**
   * Finds the on-disk `file:` URI of the materialized compiled-only outline
   * for `simpleClassName` (see [[MbtCompiledOnlyOutlineFiles]]), without
   * going through a `textDocument/definition` query -- unlike
   * [[assertOpenableDefinition]]'s caller, which would otherwise poison
   * `tables.dependencySources` for that destination (see the comment at this
   * method's call site).
   */
  private def materializedCompiledOnlyOutlineUri(
      simpleClassName: String
  )(implicit loc: munit.Location): String = {
    val root =
      workspace.resolve(Directories.dependencies).resolve("compiled-only")
    val stream = Files.walk(root.toNIO)
    try {
      val it = stream.iterator()
      var found: Option[Path] = None
      while (found.isEmpty && it.hasNext) {
        val candidate = it.next()
        if (candidate.getFileName.toString == s"$simpleClassName.java")
          found = Some(candidate)
      }
      found match {
        case Some(path) => path.toUri.toString
        case None =>
          fail(
            s"no materialized compiled-only outline found for $simpleClassName under $root"
          )
      }
    } finally stream.close()
  }

  /**
   * Goto-definition into a synthesized compiled-only outline must land on a
   * real, existing file: an in-memory-only URI reaches no editor buffer, and
   * (since it isn't inside the workspace at all) can't even be created on
   * disk if the client tries -- this is the exact failure this suite guards
   * against.
   */
  private def assertOpenableDefinition(
      locations: List[Location]
  )(implicit loc: munit.Location): Unit = {
    assert(locations.nonEmpty, "expected at least one definition location")
    locations.foreach { location =>
      val path = AbsolutePath(Paths.get(URI.create(location.getUri())))
      assert(
        path.exists,
        s"definition location does not exist on disk: $path",
      )
    }
  }

  /**
   * When the presentation compiler resolves a compiled-only class directly
   * via its synthesized outline, there is no need for -- and there must be no
   * -- separate copy decompiled through the older jar-navigation fallback.
   * A copy showing up there means `compilerDefn` came up empty and Metals
   * silently fell through to a different resolution path.
   */
  private def assertNotDecompiledAsFallback(
      simpleClassName: String
  )(implicit loc: munit.Location): Unit = {
    val decompiledRoot =
      workspace.resolve(Directories.dependencies).resolve("decompiled")
    val fallbackMaterialized =
      if (!decompiledRoot.isDirectory) false
      else {
        val stream = Files.walk(decompiledRoot.toNIO)
        try {
          val it = stream.iterator()
          var found = false
          while (!found && it.hasNext) {
            found = it.next().getFileName.toString == s"$simpleClassName.java"
          }
          found
        } finally stream.close()
      }
    assert(
      !fallbackMaterialized,
      s"expected goto-definition to resolve $simpleClassName directly via " +
        s"the synthesized outline, but it fell back to decompiling into $decompiledRoot",
    )
  }

}
