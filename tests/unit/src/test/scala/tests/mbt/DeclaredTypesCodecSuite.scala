package tests.mbt

import scala.meta.internal.metals.mbt.DeclaredTypes
import scala.meta.internal.metals.mbt.DeclaredTypesCodec

import tests.BaseSuite

/**
 * What the Turbine cache carries between sessions survives the trip.
 *
 * `TurbineCacheDeclaredTypesSuite` drives the same thing through a real cache
 * file. This covers the shapes that are harder to set up there.
 *
 * Non-ASCII characters are written as escapes to keep this file ASCII. The
 * build pins no source encoding, so a compiler reading raw UTF-8 bytes under
 * another charset would test different strings than the ones written here.
 * Each escaped word is glossed in English above the case that uses it.
 */
class DeclaredTypesCodecSuite extends BaseSuite {

  test("a-source-with-classes-and-one-without-round-trip") {
    val declared = DeclaredTypes(
      Map(
        "/a/Models.java" -> Set("a/Models", "a/Models$Inner"),
        "/a/Empty.java" -> Set.empty[String],
      )
    )

    assertEquals(roundTrip(declared), declared)
  }

  /**
   * A path can hold any of these. The encoding gives each string its own field
   * rather than separating them, so none of them is special.
   */
  test("a-path-holding-non-alphanumeric-characters-round-trips") {
    val declared = DeclaredTypes(
      Map(
        "/a/tab\there.java" -> Set("a/Tab"),
        "/a/newline\nhere.java" -> Set("a/Newline"),
        "/a/backslash\\here.java" -> Set("a/Backslash"),
        "/My Documents/a/Models.java" -> Set("a/Space"),
        "C:\\Users\\someone\\a\\Models.java" -> Set("a/WindowsPath"),
      )
    )

    assertEquals(roundTrip(declared), declared)
  }

  // Arabic: mashru = project, wahda = module, namudhaj = model
  test("arabic-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u0645\u0634\u0631\u0648\u0639",
      module = "\u0648\u062d\u062f\u0629",
      model = "\u0646\u0645\u0648\u0630\u062c",
    )
  }

  // Cyrillic: proyekt = project, modul = module, Model = model
  test("cyrillic-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u043f\u0440\u043e\u0435\u043a\u0442",
      module = "\u043c\u043e\u0434\u0443\u043b\u044c",
      model = "\u041c\u043e\u0434\u0435\u043b\u044c",
    )
  }

  // Devanagari: pariyojana = project, modyul = module, modal = model
  test("devanagari-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u092a\u0930\u093f\u092f\u094b\u091c\u0928\u093e",
      module = "\u092e\u0949\u0921\u094d\u092f\u0942\u0932",
      model = "\u092e\u0949\u0921\u0932",
    )
  }

  // Greek: ergo = project, monada = module, Montelo = model
  test("greek-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u03ad\u03c1\u03b3\u03bf",
      module = "\u03bc\u03bf\u03bd\u03ac\u03b4\u03b1",
      model = "\u039c\u03bf\u03bd\u03c4\u03ad\u03bb\u03bf",
    )
  }

  // Han: xiangmu = project, mokuai = module, moxing = model
  test("han-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u9879\u76ee",
      module = "\u6a21\u5757",
      model = "\u6a21\u578b",
    )
  }

  // Latin: made-up sequences rather than words, so one case can carry
  // diacritics from many languages at once. German umlauts and eszett with
  // Nordic letters, then Turkish dotless i and dotted capital I with cedillas
  // and a Spanish tilde, then Hungarian double acutes with French accents and
  // Czech carons, then a Romanian comma-below with a Czech caron and a Polish
  // stroke
  test("latin-with-diacritics-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\u00e4\u00f6\u00fc\u00df\u00e5\u00f8\u00e6",
      module = "\u0131\u0130\u015f\u011f\u00e7\u00f1",
      model = "\u0150\u0171\u00e9\u00e8\u0159\u017e",
      nested = Some("\u0102\u021b\u010d\u0142"),
    )
  }

  // Mathematical Alphanumeric: U+1D4D0 to U+1D4D2, each one code point that
  // UTF-16 holds as two units. The block spells no words, so letters stand in
  // for them
  test("mathematical-alphanumeric-path-and-type-name-round-trip") {
    assertRoundTrip(
      project = "\ud835\udcd0",
      module = "\ud835\udcd1",
      model = "\ud835\udcd2",
    )
  }

  /**
   * Two spellings of one accented path stay two entries, since nothing here
   * normalizes. `onDidDelete` looks a path up by exact string, so folding them
   * would let deleting one file hide the types of another.
   */
  test("two-unicode-spellings-of-one-accented-path-stay-distinct") {
    // Both render as Modele.java with a grave accent on the first e.
    val composed = "/a/Mod\u00e8le.java"
    val decomposed = "/a/Mode\u0300le.java"

    val declared = DeclaredTypes(
      Map(
        composed -> Set("a/ComposedModel"),
        decomposed -> Set("a/DecomposedModel"),
      )
    )

    val decoded = roundTrip(declared)
    assertEquals(decoded.sourcePathToTypes.size, 2)
    assertEquals(decoded.forSource(composed), Set("a/ComposedModel"))
    assertEquals(decoded.forSource(decomposed), Set("a/DecomposedModel"))
  }

  test("nothing-reads-back-as-nothing") {
    assertEquals(roundTrip(DeclaredTypes.empty), DeclaredTypes.empty)
  }

  /**
   * One source at `/project/module/Model.java` declaring `module/Model`, plus
   * `module/Model$Nested` when a nested name is given.
   */
  private def assertRoundTrip(
      project: String,
      module: String,
      model: String,
      nested: Option[String] = None,
  ): Unit = {
    val topLevel = module + "/" + model
    val typeNames = nested match {
      case Some(name) => Set(topLevel, topLevel + "$" + name)
      case None => Set(topLevel)
    }
    val declared =
      DeclaredTypes(Map(s"/$project/$module/$model.java" -> typeNames))

    assertEquals(roundTrip(declared), declared)
  }

  private def roundTrip(declared: DeclaredTypes): DeclaredTypes =
    DeclaredTypesCodec.fromBytes(DeclaredTypesCodec.toBytes(declared))
}
