package scala.meta.internal.metals.decompile

import org.eclipse.lsp4j.Location

private[decompile] final case class NavigationTarget(
    memberSymbol: String,
    classLocation: Location,
    methodDescriptor: Option[String],
)
