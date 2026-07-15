package scala.meta.internal.metals.decompile

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

/**
 * Reads the first source line (from the bytecode `LineNumberTable`) of the
 * first method named `memberName`. For a method a compiler synthesized from an
 * annotation (e.g. a Lombok `@Getter`), that line points back at the annotated
 * element in the original source, so navigation can land on real source rather
 * than decompiled output.
 *
 * Requires the class to be read with code (no `SKIP_CODE`), since line numbers
 * live in the `Code` attribute.
 */
private[decompile] final class ClassfileMemberLineVisitor(memberName: String)
    extends ClassVisitor(Opcodes.ASM9) {
  private var firstLine: Option[Int] = None

  /** The 1-based source line, if a `LineNumberTable` entry was found. */
  def line: Option[Int] = firstLine

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String],
  ): MethodVisitor =
    if (name != memberName || firstLine.isDefined) null
    else
      new MethodVisitor(Opcodes.ASM9) {
        override def visitLineNumber(line: Int, start: Label): Unit =
          if (firstLine.isEmpty) firstLine = Some(line)
      }
}
