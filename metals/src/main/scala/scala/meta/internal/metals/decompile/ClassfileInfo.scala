package scala.meta.internal.metals.decompile

/** Supertypes and declared members of a single classfile, as read by ASM. */
private final case class ClassfileInfo(
    superName: Option[String],
    interfaces: Seq[String],
    methods: Seq[(String, String)],
    fields: Seq[String],
)
