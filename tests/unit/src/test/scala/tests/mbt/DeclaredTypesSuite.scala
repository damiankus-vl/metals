package tests.mbt

import scala.collection.parallel.mutable.ParArray

import scala.meta.internal.metals.EmptyReportContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.mbt.DeclaredTypes
import scala.meta.internal.metals.mbt.TurbineCompiler
import scala.meta.pc.ProgressBars

import com.google.turbine.diag.SourceFile
import munit.Location
import tests.BaseSuite

/**
 * The classes a compilation records under the source it read them from. Deleting a
 * source hides those from CLASS_PATH, so a class left unrecorded stays resolvable after
 * its source is gone.
 *
 * Turbine is asked rather than the sources being read a second time, so the names are
 * the ones its output is listed under. `ProtoDeclaredTypesSuite` covers a `.proto` and
 * why its types cannot come from the index, `JavaPCTurbineSourcepathSuite` covers the
 * same ground end to end through what javac resolves after a delete, and
 * `DeclaredTypesCodecSuite` covers writing the result to the cache.
 */
class DeclaredTypesSuite extends BaseSuite {

  private implicit val reportContext: ReportContext = EmptyReportContext

  checkRecorded(
    name = "every-kind-of-declared-type-goes-to-its-source",
    fileName = "a/Models.java",
    source = """|package a;
                |public class Models {
                |  public static class Inner {
                |    public static class Deeper {}
                |  }
                |  public interface Listener {}
                |  public enum Kind { ONE, TWO }
                |  public @interface Marker {}
                |}
                |class Helper {}
                |interface Separate {}
                |""".stripMargin,
    expected = Set(
      "a/Models", "a/Models$Inner", "a/Models$Inner$Deeper",
      "a/Models$Listener", "a/Models$Kind", "a/Models$Marker", "a/Helper",
      "a/Separate",
    ),
  )

  // Turbine drops method bodies, so a local or anonymous class is in no output and
  // there is nothing to record for it.
  checkRecorded(
    name = "types-declared-in-a-body-are-not-recorded",
    fileName = "a/WithBodies.java",
    source = """|package a;
                |public class WithBodies {
                |  public void method() {
                |    class Local {}
                |    Runnable inner = new Runnable() {
                |      public void run() {}
                |    };
                |  }
                |}
                |""".stripMargin,
    expected = Set("a/WithBodies"),
  )

  test("a-source-nothing-compiled-has-no-types") {
    val compiled = compile(fileName = "a/Models.java", source = "package a;")

    assertEquals(compiled.forSource("a/Other.java"), Set.empty[String])
  }

  /**
   * Two sources under one path, which is what a `.proto` is. It parses to an outline
   * per generated toplevel class, and deleting it takes those outlines' classes
   * together.
   */
  test("sources-sharing-a-path-are-recorded-together") {
    val user = new SourceFile(
      "a/User.java",
      """|package a;
         |public class User {}
         |""".stripMargin,
    )
    val builder = new SourceFile(
      "a/UserOrBuilder.java",
      """|package a;
         |public interface UserOrBuilder {}
         |""".stripMargin,
    )
    val compiled = TurbineCompiler
      .compileClassfiles[Seq[SourceFile]](
        toParse = ParArray(Seq(user, builder)),
        toSourceFile = identity,
        sourcePath = _ => "/a/user.proto",
        classpath = Nil,
        progressBars = ProgressBars.EMPTY,
      )
      .declaredTypes

    assertEquals(
      compiled.forSource("/a/user.proto"),
      Set("a/User", "a/UserOrBuilder"),
    )
  }

  private def checkRecorded(
      name: String,
      fileName: String,
      source: String,
      expected: Set[String],
  )(implicit loc: Location): Unit =
    test(name) {
      val compiled = compile(fileName, source)

      assertEquals(compiled.forSource(sourcePathOf(fileName)), expected)
    }

  /** The path the fixture records under, kept apart from the source's own path. */
  private def sourcePathOf(fileName: String): String = s"/workspace/$fileName"

  private def compile(fileName: String, source: String): DeclaredTypes =
    TurbineCompiler
      .compileClassfiles[SourceFile](
        toParse = ParArray(new SourceFile(fileName, source)),
        toSourceFile = one => Seq(one),
        sourcePath = one => sourcePathOf(one.path()),
        classpath = Nil,
        progressBars = ProgressBars.EMPTY,
      )
      .declaredTypes
}
