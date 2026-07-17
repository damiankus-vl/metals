package scala.meta.internal.metals.decompile

import java.util.regex.Pattern

import org.eclipse.{lsp4j => l}

/**
 * Locates a class/interface/enum declaration by its symbol's simple name via
 * plain text search over decompiled source. Backs a defensive fallback in
 * [[scala.meta.internal.metals.Compilers.decompileAndLocate]], which always
 * decompiles a nested classpath type's *enclosing* top-level class (never
 * the nested class in isolation) so CFR renders valid Java; this only
 * matters if CFR still emits something mtags' occurrence search (which
 * requires parseable source) can't index. Also the only place that knows a
 * Scala object's `.`-suffixed symbol names a `$`-suffixed module class at the
 * JVM level (see [[simpleNameOf]]), since CFR, a Java decompiler, renders it
 * with that literal name and no notion of "object" at all.
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
   * `Inner`. A Scala object (or companion object) symbol ends in `.` rather
   * than `#`, but there's no such thing as an "object" at the JVM level: it's
   * compiled to a separate module class whose binary name carries a trailing
   * `$` (e.g. `Foo$`), which is what CFR -- a Java decompiler with no notion
   * of Scala objects -- actually renders. Appending `$` back for a `.`-ended
   * symbol matches that real declaration instead of searching for a name that
   * was never declared.
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
