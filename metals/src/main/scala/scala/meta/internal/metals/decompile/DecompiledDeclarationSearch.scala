package scala.meta.internal.metals.decompile

import java.util.regex.Pattern

import org.eclipse.{lsp4j => l}

/**
 * Locates a class/interface/enum declaration by its symbol's simple name via
 * plain text search over decompiled source. Backs a fallback in
 * [[scala.meta.internal.metals.Compilers.decompileAndLocate]] for when CFR's
 * output still isn't parseable enough for mtags' occurrence search to index.
 * Also the only place that knows a Scala object's `.`-suffixed symbol maps to
 * a `$`-suffixed module class at the JVM level (see [[simpleNameOf]]), since
 * CFR renders that literal name with no notion of "object".
 */
object DecompiledDeclarationSearch {

  /**
   * The class/interface/enum declaration in `code` matching `symbol`'s simple
   * name, or `None` if no such declaration is found there.
   */
  def declarationLocation(
      code: String,
      symbol: String,
      uri: String,
  ): Option[l.Location] = {
    val simpleName = simpleNameOf(symbol)
    if (simpleName.isEmpty) None
    else {
      val pattern =
        raw"(?<![\w$$])(?:class|interface|enum)\s+(?:[\w$$]+\.)*(${Pattern
            .quote(simpleName)})(?![\w$$])".r
      pattern
        .findFirstMatchIn(code)
        .map(m =>
          new l.Location(
            uri,
            new l.Range(
              offsetToPosition(code, m.start(1)),
              offsetToPosition(code, m.end(1)),
            ),
          )
        )
    }
  }

  /**
   * The trailing simple name of a SemanticDB symbol, e.g. `Outer#Inner#` ->
   * `Inner`. Object symbols end in `.` rather than `#`; since an object
   * compiles to a `$`-suffixed module class that CFR renders literally, `$`
   * is appended back so the search matches what's actually declared.
   */
  private def simpleNameOf(symbol: String): String = {
    val isModule = symbol.endsWith(".")
    val name =
      symbol
        .stripSuffix("#")
        .stripSuffix(".")
        .reverse
        .takeWhile(c => c != '#' && c != '/' && c != '.')
        .reverse
    if (isModule) s"$name$$" else name
  }

  private def offsetToPosition(text: String, offset: Int): l.Position = {
    val before = text.substring(0, offset)
    new l.Position(
      before.count(_ == '\n'),
      offset - before.lastIndexOf('\n') - 1,
    )
  }
}
