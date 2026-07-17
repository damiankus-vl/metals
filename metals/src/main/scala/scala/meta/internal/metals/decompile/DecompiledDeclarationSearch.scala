package scala.meta.internal.metals.decompile

import java.util.regex.Pattern

import org.eclipse.{lsp4j => l}

/**
 * Locates a class/interface/enum declaration by its symbol's simple name via
 * plain text search over decompiled source. Backs the fallback in
 * [[scala.meta.internal.metals.Compilers.decompileAndLocate]] for a nested
 * classpath type CFR decompiled in isolation from its enclosing class: CFR
 * labels the declaration `class Outer.Inner`, which is not valid Java, so the
 * usual mtags-based occurrence search (which requires parseable source)
 * never finds a matching definition.
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

  /** The trailing simple name of a SemanticDB symbol, e.g. `Outer#Inner#` -> `Inner`. */
  private def simpleNameOf(symbol: String): String =
    symbol
      .stripSuffix("#")
      .stripSuffix(".")
      .reverse
      .takeWhile(c => c != '#' && c != '/' && c != '.')
      .reverse

  private def offsetToPosition(text: String, offset: Int): l.Position = {
    val before = text.substring(0, offset)
    new l.Position(
      before.count(_ == '\n'),
      offset - before.lastIndexOf('\n') - 1,
    )
  }
}
