package scala.meta.internal.metals.decompile

import scala.collection.mutable

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes

/** Minimal ASM visitor: supertypes and declared member names/descriptors. */
private final class ClassfileInfoVisitor extends ClassVisitor(Opcodes.ASM9) {
  private var superName: Option[String] = None
  private var interfaces: Seq[String] = Nil
  private val methods = mutable.ListBuffer.empty[(String, String)]
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
      if (interfaces == null) Nil else interfaces.toIndexedSeq
  }

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String],
  ): MethodVisitor = {
    methods += name -> descriptor
    null
  }

  override def visitField(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      value: Any,
  ): FieldVisitor = {
    fields += name
    null
  }

  def result: ClassfileInfo =
    ClassfileInfo(superName, interfaces, methods.toList, fields.toList)
}
