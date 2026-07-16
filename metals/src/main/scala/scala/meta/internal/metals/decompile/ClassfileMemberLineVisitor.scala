package scala.meta.internal.metals.decompile

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

private object ClassfileMemberLineVisitor {
  private val NoOpMethodVisitor: MethodVisitor = new MethodVisitor(
    Opcodes.ASM9
  ) {}
}

/**
 * Reads the first source line (from the bytecode `LineNumberTable`) of a method
 * named `memberName`. When `methodDescriptor` is given, only the overload with
 * that JVM descriptor is matched, so overloads sharing a name resolve to their
 * own lines rather than all landing on whichever the class file listed first;
 * with `None` the first method of that name is used. For a method a compiler
 * synthesized from an annotation (e.g. a Lombok `@Getter`), that line points
 * back at the annotated element in the original source, so navigation can land
 * on real source rather than decompiled output.
 *
 * Requires the class to be read with code (no `SKIP_CODE`), since line numbers
 * live in the `Code` attribute.
 */
private[decompile] final class ClassfileMemberLineVisitor(
    memberName: String,
    methodDescriptor: Option[String],
) extends ClassVisitor(Opcodes.ASM9) {
  import ClassfileMemberLineVisitor._

  private var firstLine: Option[Int] = None

  /** The 1-based source line, if a `LineNumberTable` entry was found. */
  def line: Option[Int] = firstLine

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String],
  ): MethodVisitor = {
    if (
      name != memberName ||
      methodDescriptor.exists(_ != descriptor) ||
      firstLine.isDefined
    ) {
      NoOpMethodVisitor
    } else {
      new MethodVisitor(Opcodes.ASM9) {
        override def visitLineNumber(line: Int, start: Label): Unit = {
          if (firstLine.isEmpty) {
            firstLine = Some(line)
          }
        }
      }
    }
  }
}
