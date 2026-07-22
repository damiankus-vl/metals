package scala.meta.internal.metals.decompile

import scala.collection.mutable

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

private final class ClassFileInfoVisitor extends ClassVisitor(Opcodes.ASM9) {
  private var superName: Option[String] = None
  private var interfaces: Seq[String] = Nil
  private val methods = mutable.ListBuffer.empty[MethodInfo]
  private val fields = mutable.ListBuffer.empty[String]

  override def visit(
      version: Int,
      access: Int,
      name: String,
      signature: String,
      superName: String,
      interfaces: Array[String],
  ): Unit = {
    this.superName = Option(superName)
    this.interfaces =
      if (interfaces == null) {
        Nil
      } else {
        interfaces.toIndexedSeq
      }
  }

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String],
  ): MethodVisitor = {
    methods += MethodInfo(name, descriptor)
    Asm.skipVisit
  }

  override def visitField(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      value: Any,
  ): FieldVisitor = {
    fields += name
    Asm.skipVisit
  }

  def result: ClassFileInfo =
    ClassFileInfo(superName, interfaces, methods.toList, fields.toList)
}
