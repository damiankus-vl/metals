// In `scala.tools.nsc` beside `ParsedLogicalPackage`. That is what serves it to
// the compiler. Nothing package-private is needed here.
package scala.tools.nsc

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets

import scala.reflect.io.VirtualFile

/**
 * A source the compiler reads from memory, for code Metals synthesizes rather
 * than writes, such as a Java outline generated from a `.proto`.
 *
 * `name` must keep the extension, `User.java` not `User`.
 * `ClassRepresentation.name` rejects a name without one, and
 * `CompilationUnit.isJava` reads it to pick a parser.
 *
 * `path` is where the file would live if written,
 * `/w/.metals/readonly/dependencies/proto-generated/a/model.proto/User.java`.
 * It keys `compiledFiles` and `VirtualFile` equality. A diagnostic prints it.
 * It has to be the path the rest of Metals uses.
 */
final class InMemorySourceFile(
    name: String,
    path: String,
    text: String
) extends VirtualFile(name, path) {

  // `VirtualFile` stores a byte array written through `output`. This holds the
  // text instead and encodes it when the compiler opens the file. The compiler
  // opens a small part of a workspace-wide source path.
  private lazy val bytes: Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  override def unsafeToByteArray: Array[Byte] = bytes

  override def input: InputStream = new ByteArrayInputStream(bytes)

  // The byte count, not `text.length`. A `.proto` comment carried into the
  // outline can hold a character UTF-8 spends more than one byte on, and
  // `toByteArray` sizes its buffer from this.
  override def sizeOption: Option[Int] = Some(bytes.length)

  // `lastModified` is left at the inherited 0.
  // `JavaPlatform.needCompile` picks a compiled class over a source when
  // `src.lastModified >= bin.lastModified`. Dated 1970 this stand-in loses.
  // A compiled class carries more than Metals could infer from the `.proto`.
}
