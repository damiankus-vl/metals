package scala.meta.internal.metals.mbt

/**
 * The types Turbine compiled, under the path of the source it read them from, with `/`
 * between the package parts and `$` between the nesting levels. A source that produced
 * no type is absent.
 *
 * Types rather than classes, since interfaces, enums, records and annotation types are
 * all in here. The JVM loads a classfile for each and CLASS_PATH lists them the same
 * way, so hiding one is hiding any of them.
 *
 * Deleting a source hides its types from CLASS_PATH until a compilation rebuilds
 * without them. They come from the compilation that produced the output being served,
 * so they are the names that output is listed under and cannot be spelled differently
 * from it.
 *
 * A `.proto` is one such path, and the types of the outlines generated from it are
 * recorded under it, since deleting the proto takes them together.
 */
final case class DeclaredTypes(
    sourcePathToTypes: Map[String, Set[String]]
) {

  /** Empty for a source the compilation behind the current output did not read. */
  def forSource(sourcePath: String): Set[String] =
    sourcePathToTypes.getOrElse(sourcePath, Set.empty)
}

object DeclaredTypes {
  val empty: DeclaredTypes = DeclaredTypes(Map.empty)
}
