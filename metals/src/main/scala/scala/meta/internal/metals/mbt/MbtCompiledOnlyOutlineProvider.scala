package scala.meta.internal.metals.mbt

import java.net.URI
import java.nio.file.Files
import java.util.concurrent.ConcurrentHashMap
import java.{lang => jl}
import java.{util => ju}
import javax.tools.ForwardingJavaFileManager
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.JavaFileObject.Kind
import javax.tools.SimpleJavaFileObject

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.decompile.CfrDecompiler
import scala.meta.io.AbsolutePath
import scala.meta.pc
import scala.meta.pc.SemanticdbCompilationUnit

import ch.epfl.scala.bsp4j.BuildTargetIdentifier

/**
 * A Java source outline for a class that exists only in a build target's
 * real compiled output (no `.java` source anywhere) -- e.g. an
 * annotation-processor-generated companion class (AutoValue, Lombok, ...).
 * Decompiled from bytecode via CFR lazily on first read, then cached, so the
 * cost is only paid for classes javac actually ends up parsing rather than
 * every class enumerated in the package.
 *
 * Feeding this through the virtual `-sourcepath` (rather than putting the
 * real `.class`/jar on `-classpath`) also sidesteps javac's strict
 * class-file-version gate, which otherwise refuses to read bytecode newer
 * than the running JDK -- CFR parses raw bytecode independently of that.
 */
final class CompiledOnlyOutlineFile(
    workspace: AbsolutePath,
    buildTargetId: String,
    binaryClassName: String,
    packageName: String,
    classpath: List[AbsolutePath],
    decompiler: CfrDecompiler,
) extends SimpleJavaFileObject(
      MbtCompiledOnlyOutlineFiles
        .pathFor(workspace, buildTargetId, binaryClassName)
        .toURI,
      Kind.SOURCE,
    )
    with SemanticdbCompilationUnit {

  def simpleName: String =
    binaryClassName.substring(binaryClassName.lastIndexOf('.') + 1)

  override def language(): pc.Language = pc.Language.JAVA
  override def binaryName(): String = binaryClassName
  override def packageSymbols(): ju.List[String] =
    ju.Collections.singletonList(packageName)
  override def toplevelSymbols(): ju.List[String] =
    ju.Collections.singletonList(
      binaryClassName.replace('.', '/') + "#"
    )
  override def uri(): URI = toUri()
  override def text(): String = decompiledText
  override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence =
    decompiledText

  private lazy val decompiledText: String = {
    val source = Await.result(
      decompiler.decompile(binaryClassName, classpath),
      30.seconds,
    ) match {
      case Right(source) => source
      case Left(error) =>
        scribe.error(
          s"mbt-compiled-only-outline: failed to decompile $binaryClassName: $error"
        )
        s"package ${packageName.replace('/', '.')};\n"
    }
    // Materialized to a real file so goto-definition into this outline opens
    // it like any other dependency source, instead of an in-memory-only URI
    // the editor can neither read nor (its filesystem being read-only from
    // the client's perspective) create.
    MbtCompiledOnlyOutlineFiles.materialize(
      workspace,
      buildTargetId,
      binaryClassName,
      source,
    )
    source
  }
}

/**
 * Synthesizes [[CompiledOnlyOutlineFile]] outlines for a build target's own
 * compiled output, keyed by package. See [[MbtBuild.classDirectoriesFor]]
 * for where the target's real compiled-output jars/dirs come from.
 */
final class MbtCompiledOnlyOutlineProvider(
    mbtBuild: () => MbtBuild,
    workspace: AbsolutePath,
) {
  private val decompiler = new CfrDecompiler
  private val cache =
    new ConcurrentHashMap[(String, String), JavaFileObject]()

  /**
   * Synthesized outlines for every top-level, compiled-only class declared
   * directly in `packageName` (slash-separated, no trailing slash) for
   * `buildTargetId`. `dependencyClasspath` is passed along to CFR so it can
   * resolve types referenced by the decompiled class.
   */
  def outlinesForPackage(
      buildTargetId: String,
      packageName: String,
      dependencyClasspath: Seq[AbsolutePath],
  ): Iterator[JavaFileObject] = {
    val classDirs =
      mbtBuild().classDirectoriesFor(
        new BuildTargetIdentifier(buildTargetId),
        workspace,
      )
    if (classDirs.isEmpty) Iterator.empty
    else {
      val fullClasspath = (classDirs ++ dependencyClasspath).toList
      for {
        classDir <- classDirs.iterator
        simpleName <- classNamesInPackage(classDir, packageName).iterator
      } yield outlineFor(buildTargetId, packageName, simpleName, fullClasspath)
    }
  }

  private def outlineFor(
      buildTargetId: String,
      packageName: String,
      simpleName: String,
      classpath: List[AbsolutePath],
  ): JavaFileObject = {
    val binaryName =
      if (packageName.isEmpty) simpleName
      else s"${packageName.replace('/', '.')}.$simpleName"
    cache.computeIfAbsent(
      (buildTargetId, binaryName),
      _ =>
        new CompiledOnlyOutlineFile(
          workspace,
          buildTargetId,
          binaryName,
          packageName,
          classpath,
          decompiler,
        ),
    )
  }

  /** Top-level (non-nested, non-anonymous) `.class` simple names directly under `packageName`. */
  private def classNamesInPackage(
      classDir: AbsolutePath,
      packageName: String,
  ): List[String] =
    try {
      if (classDir.filename.endsWith(".jar")) {
        FileIO.withJarFileSystem(classDir, create = false) { root =>
          listClassNames(root.resolve(packageName))
        }
      } else if (classDir.isDirectory) {
        listClassNames(classDir.resolve(packageName))
      } else Nil
    } catch {
      case NonFatal(_) => Nil
    }

  private def listClassNames(dir: AbsolutePath): List[String] = {
    if (!Files.isDirectory(dir.toNIO)) Nil
    else {
      val stream = Files.list(dir.toNIO)
      try {
        val builder = List.newBuilder[String]
        val it = stream.iterator()
        while (it.hasNext) {
          val name = it.next().getFileName.toString
          // A `$` past the first character marks a nested/anonymous class
          // (`Outer$Inner.class`) to skip. A LEADING `$` is not a nested-class
          // separator -- it's a top-level class, e.g. AutoValue's `$AutoValue_Foo`
          // abstract base for `@Memoized`-annotated value classes -- and must be
          // kept, or a sibling outline `extends`-ing it can't fully resolve.
          if (name.endsWith(".class") && name.indexOf('$', 1) < 0)
            builder += name.stripSuffix(".class")
        }
        builder.result()
      } finally stream.close()
    }
  }
}

/**
 * Wraps a build target's `JavaFileManager` so that `SOURCE_PATH` listings
 * also include synthesized outlines for that target's compiled-only classes
 * (see [[MbtCompiledOnlyOutlineProvider]]). A class that already has a real
 * source in the delegate's listing is never duplicated -- only classes with
 * no real source at all get a synthesized entry appended.
 */
final class CompiledOnlyOutlineFileManager(
    delegate: JavaFileManager,
    buildTargetId: String,
    dependencyClasspath: List[AbsolutePath],
    outlineProvider: MbtCompiledOnlyOutlineProvider,
) extends ForwardingJavaFileManager[JavaFileManager](delegate) {

  override def list(
      location: JavaFileManager.Location,
      packageName: String,
      kinds: ju.Set[JavaFileObject.Kind],
      recurse: Boolean,
  ): jl.Iterable[JavaFileObject] = {
    val real = super.list(location, packageName, kinds, recurse)
    if (location.getName() != "SOURCE_PATH") real
    else {
      val result = new ju.ArrayList[JavaFileObject]()
      val realSimpleNames = new ju.HashSet[String]()
      val realIt = real.iterator()
      while (realIt.hasNext) {
        val file = realIt.next()
        result.add(file)
        realSimpleNames.add(simpleNameOf(file))
      }
      val extraIt = outlineProvider
        .outlinesForPackage(
          buildTargetId,
          packageName.replace('.', '/'),
          dependencyClasspath,
        )
      while (extraIt.hasNext) {
        extraIt.next() match {
          case outline: CompiledOnlyOutlineFile
              if !realSimpleNames.contains(outline.simpleName) =>
            result.add(outline)
          case _ =>
        }
      }
      result
    }
  }

  private def simpleNameOf(file: JavaFileObject): String = {
    val name = file.getName
    val base = name.substring(name.lastIndexOf('/') + 1)
    base.stripSuffix(".java")
  }
}
