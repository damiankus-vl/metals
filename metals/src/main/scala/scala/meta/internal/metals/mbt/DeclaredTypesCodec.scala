package scala.meta.internal.metals.mbt

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.jturbine.Turbine

/**
 * Writes [[DeclaredTypes]] into the Turbine cache and reads them back.
 *
 * The encoding is the `DeclaredTypes` message of `turbine.proto`, which is how the
 * rest of what Metals persists about a workspace is written too. Nothing here separates
 * or escapes anything, so a path holding a tab, a newline or a backslash is just a path.
 */
object DeclaredTypesCodec {

  def toBytes(declared: DeclaredTypes): Array[Byte] = {
    val sources = declared.sourcePathToTypes.map {
      case (sourcePath, typeNames) =>
        Turbine.DeclaredTypes.Source
          .newBuilder()
          .setPath(sourcePath)
          .addAllTypeName(typeNames.asJava)
          .build()
    }
    Turbine.DeclaredTypes
      .newBuilder()
      .addAllSources(sources.asJava)
      .build()
      .toByteArray()
  }

  def fromBytes(bytes: Array[Byte]): DeclaredTypes = {
    val sourcePathToTypes = Turbine.DeclaredTypes
      .parseFrom(bytes)
      .getSourcesList()
      .asScala
      .map(source => source.getPath() -> source.getTypeNameList().asScala.toSet)
    DeclaredTypes(sourcePathToTypes.toMap)
  }
}
