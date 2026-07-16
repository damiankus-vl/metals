package scala.meta.internal.metals.decompile

private final case class ClassFileInfo(
    superName: Option[String],
    interfaces: Seq[String],
    methods: Seq[MethodInfo],
    fields: Seq[String],
)
