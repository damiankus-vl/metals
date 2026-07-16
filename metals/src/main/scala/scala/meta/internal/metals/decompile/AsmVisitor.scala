package scala.meta.internal.metals.decompile

object AsmVisitor {

  /**
   * According to the Javadoc of:
   * - [[org.objectweb.asm.ClassVisitor#visitMethod]] and
   * - [[org.objectweb.asm.ClassVisitor#visitField]]
   * we should return null if we don't want to visit the code of type members
   */
  val skipVisit: Null = null
}
