package scala.meta.internal.metals

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.util.Try

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.Configs.FallbackClasspathConfig
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.mbt.MbtBuild
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import ch.epfl.scala.bsp4j.MavenDependencyModule

trait BaseFallbackClasspaths {
  def javaCompilerClasspath(): Seq[Path]
  def scalaCompilerClasspath(): Seq[Path]

  /**
   * Compiled workspace output (jars or directories) declared by the build,
   * for read-only navigation into compiled-only members. Kept separate from
   * the compiler classpaths, since a compiler must never see its own output.
   */
  def classDirectories(): Seq[Path] = Nil
}
object EmptyFallbackClasspaths extends BaseFallbackClasspaths {
  override def javaCompilerClasspath(): Seq[Path] = Nil
  override def scalaCompilerClasspath(): Seq[Path] = Nil
}

/**
 * Infers the classpath to use for the fallback compiler.
 *
 * In particular, we want to avoid mixing Scala 2.12 and 2.13 dependencies on
 * the same classpath.
 *
 * - For Scala, we only include jars if they're a dependency of a target with a
 *   matching `scalaBinaryVersion` as the user's configured
 *   `fallbackScalaVersion`.
 * - For Java, we only include jars if they're a dependency of another Java
 *   target *and*, if the dependency module looks like a Scala target
 *   (org.scala-lang org, or _2.12/_2.13/_3 suffix in the name), then we only
 *   include it if it matches the user's configured `fallbackScalaVersion`.
 */
class FallbackClasspaths(
    workspace: AbsolutePath,
    buildTargets: BuildTargets = BuildTargets.empty,
    fallbackClasspathsConfig: () => FallbackClasspathConfig = () =>
      FallbackClasspathConfig.default,
    scalaVersionSelector: ScalaVersionSelector = ScalaVersionSelector.default,
    mbtBuild: () => MbtBuild,
) extends BaseFallbackClasspaths {
  private def fallbackCompilerClasspath(
      includeBuildTarget: BuildTargetIdentifier => Boolean,
      includeModule: MavenDependencyModule => Boolean,
  ): Seq[Path] = {
    val bspClasspath: Seq[Path] =
      if (fallbackClasspathsConfig().isAll3rdparty) {
        Seq.from(
          buildTargets
            .allDependencyModuleArtifacts(includeBuildTarget, includeModule)
            .map(_.toNIO)
        )
      } else {
        // Assumes we're auto-including scala-library from elsewhere. That logic
        // should ideally be moved into this method so we have one source of truth
        // for what's on the classpath of the fallback compiler.
        Nil
      }
    if (bspClasspath.isEmpty) {
      val paths = mbtClasspath()
      scribe.debug(
        s"fallback-classpath: mbt contributed ${paths.size} jars"
      )
      paths
    } else
      bspClasspath
  }
  def javaCompilerClasspath(): Seq[Path] = {
    val scalaBinaryVersion = fallbackScalaBinaryVersion()
    def inferScalaBinaryVersion(
        module: MavenDependencyModule
    ): String = {
      if (module.getOrganization() == "org.scala-lang") {
        ScalaVersions.scalaBinaryVersionFromFullVersion(module.getVersion())
      } else if (module.getName().endsWith("_3")) {
        "3"
      } else if (module.getName().endsWith("_2.13")) {
        "2.13"
      } else if (module.getName().endsWith("_2.12")) {
        "2.12"
      } else {
        // Technically: None.
        scalaBinaryVersion
      }
    }
    val result = fallbackCompilerClasspath(
      id =>
        // Technically, we could pick jars from one of Scala 2.12/2.13 targets, but
        // we have no guarantee that the `scalaFallbackVersion` in the user's settings
        // mirrors the Scala version that is allowed as a dependency in Java targets.
        buildTargets.jvmTarget(id).isDefined,
      module => inferScalaBinaryVersion(module) == scalaBinaryVersion,
    )
    if (result.isEmpty) {
      guessClasspath() ++ mbtClasspath()
    } else {
      result
    }
  }
  private def fallbackScalaBinaryVersion(): String = {
    val scalaVersion =
      scalaVersionSelector.fallbackScalaVersion()
    ScalaVersions.scalaBinaryVersionFromFullVersion(scalaVersion)
  }
  def scalaCompilerClasspath(): Seq[Path] = {
    val scalaBinaryVerion = fallbackScalaBinaryVersion()
    fallbackCompilerClasspath(
      id =>
        // IMPORTANT: we must only include dependencies from targets that have a
        // compatible Scala version.  If we include, for example, Java targets then
        // we may implicitly pull in Scala dependencies from an incompatible binary
        // version.
        buildTargets
          .scalaTarget(id)
          .exists(_.scalaBinaryVersion == scalaBinaryVerion),
      _ => true,
    )
  }

  private def mbtClasspath(): Seq[Path] = {
    if (!fallbackClasspathsConfig().isMbt) {
      return Nil
    }
    val build = mbtBuild()
    build.getDependencyModules.asScala.iterator.flatMap(_.jarPath).toSeq
  }

  // `MbtBuild.allClassDirectories` fully rebuilds every namespace's target
  // info on every access (dependency-graph resolution, glob matcher
  // compilation, re-emitting "unknown dependency module" warnings) -- and
  // this method is called on every `decompileAndLocate` (goto-definition
  // into any external/decompiled classpath-only class). Cache the result,
  // invalidated by `MbtBuild` reference identity: a reimport replaces the
  // whole instance rather than mutating one in place, so this needs no
  // explicit invalidation hook. `@volatile` for safe cross-thread publication
  // of this single-slot cache; a lost race just recomputes once more.
  @volatile private var classDirectoriesCache: Option[(MbtBuild, Seq[Path])] =
    None

  override def classDirectories(): Seq[Path] =
    if (fallbackClasspathsConfig().isMbt) {
      val build = mbtBuild()
      classDirectoriesCache match {
        case Some((cachedBuild, dirs)) if cachedBuild eq build => dirs
        case _ =>
          val dirs = build.allClassDirectories(workspace).map(_.toNIO)
          classDirectoriesCache = Some((build, dirs))
          dirs
      }
    } else Nil

  private def guessClasspath(): Seq[Path] = {
    if (!fallbackClasspathsConfig().isGuessed) {
      return Nil
    }

    val bazelbsp = workspace.resolve(".bazelbsp/artifacts")
    val bloop = workspace.resolve(".bloop")

    if (bazelbsp.isDirectory) {
      FileIO
        .listAllFilesRecursively(bazelbsp)
        .iterator
        .filter(_.isFile)
        .filter(_.extension == "jar")
        .filter(file =>
          file.toString.contains("third_party") ||
            file.toString.contains("2.12") ||
            file.toString.contains("2.13") ||
            file.toString.contains("shaded")
        )
        .filterNot(_.filename.endsWith("-sources.jar"))
        .map(_.toNIO)
        .toSeq
    } else if (bloop.isDirectory) {
      (for {
        file <- bloop.list.toSeq.iterator
        if file.isFile && file.extension == "json"
        text <- file.readTextOpt.iterator
        json <- Try(ujson.read(text)).toOption.iterator
        project <- json.objOpt.flatMap(_.get("project")).iterator
        classpathEntry <- project.objOpt.flatMap(_.get("classpath")).iterator
        entries <- classpathEntry.arrOpt.iterator
        entry <- entries.iterator.flatMap(_.strOpt.iterator)
        if entry.endsWith(".jar")
      } yield Paths.get(entry)).distinct
        .filter(path => Files.exists(path))
        .toSeq
    } else {
      Nil
    }
  }
}
