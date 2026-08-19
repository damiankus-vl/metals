package tests.mbt

import java.nio.file.Files

import scala.collection.parallel.mutable.ParArray

import scala.meta.internal.metals.Configs
import scala.meta.internal.metals.EmptyReportContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.mbt.DeclaredTypes
import scala.meta.internal.metals.mbt.TurbineCache
import scala.meta.internal.metals.mbt.TurbineCompileResult
import scala.meta.internal.metals.mbt.TurbineCompiler
import scala.meta.io.AbsolutePath
import scala.meta.pc.ProgressBars

import com.google.turbine.diag.SourceFile
import tests.BaseSuite

/**
 * The classes a compilation recorded under a source survive a trip through the cache
 * file.
 *
 * A session that starts from the cache has run no compilation of its own, so this is
 * the only thing a deletion in it has to go on. `TurbineCacheSuite` cannot cover it:
 * the cache is only read when recompilation is enabled, and a recompile then drops the
 * deleted classes on its own, whatever the cache said.
 */
class TurbineCacheDeclaredTypesSuite extends BaseSuite {

  private implicit val reportContext: ReportContext = EmptyReportContext

  private val declaringSource = "/workspace/a/Models.java"

  test("the-classes-recorded-under-a-source-survive-the-cache") {
    val workspace = temporaryWorkspace()
    val cache = newCache(workspace)

    cache.writeCache(compiledModels)
    val cached = cache.readCache(Nil)

    assertEquals(
      cached.map(_.declaredTypes.forSource(declaringSource)),
      Some(Set("a/Models", "a/Models$Inner", "a/Helper")),
    )
  }

  /**
   * A cache written before the classes were stored alongside them. It still serves its
   * classfiles, and records nothing rather than failing to load.
   */
  test("a-cache-without-them-loads-with-nothing-recorded") {
    val workspace = temporaryWorkspace()
    val cache = newCache(workspace)

    cache.writeCache(
      compiledModels.copy(declaredTypes = DeclaredTypes.empty)
    )
    val cached = cache.readCache(Nil)

    assert(cached.nonEmpty, "the cache did not load at all")
    assertEquals(cached.map(_.declaredTypes), Some(DeclaredTypes.empty))
    assert(
      cached.exists(_.lowered.bytes().containsKey("a/Models")),
      "the cached classes did not come back",
    )
  }

  private lazy val compiledModels: TurbineCompileResult =
    TurbineCompiler.compileClassfiles[SourceFile](
      toParse = ParArray(
        new SourceFile(
          "a/Models.java",
          """|package a;
             |public class Models {
             |  public static class Inner {}
             |}
             |class Helper {}
             |""".stripMargin,
        )
      ),
      toSourceFile = one => Seq(one),
      sourcePath = _ => declaringSource,
      classpath = Nil,
      progressBars = ProgressBars.EMPTY,
    )

  /**
   * The cache is keyed by the git HEAD, which it is handed rather than reading, so a
   * fixed one stands in for a repository here.
   */
  private def newCache(workspace: AbsolutePath): TurbineCache =
    new TurbineCache(
      workspace,
      cacheConfig = () => Configs.TurbineCacheConfig.enabled,
      recompileDelayConfig = () => Configs.TurbineRecompileDelayConfig.testing,
      time = Time.system,
      headHash = () => Some("0f1e2d3c4b5a"),
    )

  private def temporaryWorkspace(): AbsolutePath = {
    val directory = Files.createTempDirectory("turbine-cache-declared-classes")
    directory.toFile().deleteOnExit()
    AbsolutePath(directory)
  }
}
