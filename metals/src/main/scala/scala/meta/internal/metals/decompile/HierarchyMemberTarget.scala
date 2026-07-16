package scala.meta.internal.metals.decompile

import org.eclipse.{lsp4j => l}

/**
 * A navigation target for a member found while walking the compiled type
 * hierarchy.
 *
 * @param memberSymbol
 *   the member symbol rebased onto the class that declares it (methods carry an
 *   overload disambiguator, `().`/`(+N).`).
 * @param classLocation
 *   the `.class` location of that declaring class, to be decompiled or replaced
 *   by a workspace-source location.
 * @param methodDescriptor
 *   the JVM descriptor of the specific method overload, or `None` for a field.
 *   It distinguishes overloads that share a name so each resolves its own
 *   source line instead of collapsing onto whichever the class file listed
 *   first.
 */
private[decompile] final case class HierarchyMemberTarget(
    memberSymbol: String,
    classLocation: l.Location,
    methodDescriptor: Option[String],
)
