package scala.meta.internal.metals

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.clients.language.MetalsLanguageClient

import org.eclipse.lsp4j.MessageActionItem
import org.eclipse.lsp4j.MessageType
import org.eclipse.lsp4j.ShowMessageRequestParams

/**
 * Gates decompilation of a library's compiled `.class` file behind user
 * consent. Decompiled output reconstructs source from bytecode and may be
 * restricted by the library's license, so the user is asked to confirm they
 * are permitted to view it.
 *
 * A single goto-definition decompiles the class twice (once to find the line
 * to jump to, once to render its contents), so consent is shared: "Proceed"
 * allows decompilation for the current session and "Always allow in this
 * workspace" persists it, ensuring the user is prompted at most once per
 * session rather than once per decompilation.
 */
class DecompilationConsent(
    languageClient: MetalsLanguageClient,
    tables: Tables,
)(implicit ec: ExecutionContext) {

  private val consentedThisSession = new AtomicBoolean(false)

  private def isGranted: Boolean =
    consentedThisSession.get() ||
      tables.dismissedNotifications.DecompilationConsent.isDismissed

  /** Whether decompilation may proceed, prompting the user if not yet decided. */
  def ensureConsent(): Future[Boolean] = {
    if (isGranted) Future.successful(true)
    else {
      val proceed = new MessageActionItem("Proceed")
      val alwaysInWorkspace =
        new MessageActionItem("Always allow in this workspace")
      val params = new ShowMessageRequestParams()
      params.setType(MessageType.Warning)
      params.setMessage(
        "Metals is about to decompile a compiled .class file into readable " +
          "source. Decompiled output is derived from the library's bytecode " +
          "and may be subject to its license terms. Only proceed if you are " +
          "permitted to view this library's source (for example it is open " +
          "source, or you hold a license granting that right). Metals cannot " +
          "verify your eligibility."
      )
      params.setActions(List(proceed, alwaysInWorkspace).asJava)
      languageClient.showMessageRequest(params).asScala.map { item =>
        if (item == alwaysInWorkspace) {
          tables.dismissedNotifications.DecompilationConsent.dismissForever()
          consentedThisSession.set(true)
          true
        } else if (item == proceed) {
          consentedThisSession.set(true)
          true
        } else false
      }
    }
  }
}
