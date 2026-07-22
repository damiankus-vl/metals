package scala.meta.internal.metals.decompile

object Asm {

  /**
   * `null` tells ASM to skip visiting a member's body, per the Javadoc of
   * [[org.objectweb.asm.ClassVisitor#visitMethod]] and
   * [[org.objectweb.asm.ClassVisitor#visitField]].
   */
  val skipVisit: Null = null
}
