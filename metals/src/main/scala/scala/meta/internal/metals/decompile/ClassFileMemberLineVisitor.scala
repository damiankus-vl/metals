package scala.meta.internal.metals.decompile

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

private[decompile] final class ClassFileMemberLineVisitor(
    memberName: String,
    methodDescriptor: Option[String],
) extends ClassVisitor(Opcodes.ASM9) {

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
      firstLine.isEmpty &&
      name == memberName &&
      methodDescriptor.forall(_ == descriptor)
    ) {
      new MethodVisitor(Opcodes.ASM9) {
        override def visitLineNumber(line: Int, start: Label): Unit = {
          if (firstLine.isEmpty) {
            firstLine = Some(line)
          }
        }
      }
    } else {
      Asm.skipVisit
    }
  }
}
