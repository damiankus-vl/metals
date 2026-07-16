package scala.meta.internal.metals

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.control.NonFatal

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
 * Consent is granted per session, not per file or per class: the first answer
 * governs every later decompilation until the server restarts. A single
 * goto-definition decompiles the class twice (once to find the line to jump to,
 * once to render its contents), so consent is shared: "Allow for this session"
 * allows decompilation for the current session and "Always allow in this
 * workspace" persists it across restarts, ensuring the user is prompted at most
 * once per session rather than once per decompilation.
 */
class DecompilationConsent(
    languageClient: MetalsLanguageClient,
    tables: Tables,
)(implicit ec: ExecutionContext) {

  private val consentedThisSession = new AtomicBoolean(false)

  // The session's answer once the user has decided, shared across all callers.
  // Reusing it means a decline (as well as a grant) sticks for the session, so
  // a second decompile-gated call in the same request — e.g. the standard
  // definition path and this fallback both reaching a `.class` — never opens a
  // second prompt for the same class. While a prompt is still on screen it also
  // lets concurrent decompile attempts reuse the one prompt. Guarded by `this`;
  // cleared only when the prompt fails or times out, so an unanswered prompt can
  // be retried by a later navigation. A grant additionally sets
  // `consentedThisSession`, so `isGranted` short-circuits without consulting it.
  private var decision: Option[Future[Boolean]] = None

  private def isGranted: Boolean =
    consentedThisSession.get() ||
      tables.dismissedNotifications.DecompilationConsent.isDismissed

  /** Whether decompilation may proceed, prompting the user if not yet decided. */
  def ensureConsent(): Future[Boolean] = {
    if (isGranted) Future.successful(true)
    else {
      val prompt = synchronized {
        decision.getOrElse {
          val fresh = requestConsent()
          decision = Some(fresh)
          fresh.onComplete {
            case Failure(_) =>
              synchronized { if (decision.contains(fresh)) decision = None }
            case _ => ()
          }
          fresh
        }
      }
      // A prompt the client never answers (dialog dismissed, or a headless
      // client that doesn't implement `window/showMessageRequest`) times out
      // in `requestConsent` and fails the future; recover to "not granted" so
      // it doesn't hang every other decompile-gated call. The timeout is not
      // cached (see the `onComplete` above), so navigation can prompt again.
      prompt.recover { case NonFatal(_) => false }
    }
  }

  private def requestConsent(): Future[Boolean] = {
    val allowThisSession =
      new MessageActionItem(DecompilationConsent.allowThisSessionTitle)
    val alwaysInWorkspace =
      new MessageActionItem(DecompilationConsent.alwaysInWorkspaceTitle)
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
    params.setActions(List(allowThisSession, alwaysInWorkspace).asJava)
    languageClient
      .showMessageRequest(params)
      .asScala
      .withTimeout(
        DecompilationConsent.promptTimeoutMinutes,
        TimeUnit.MINUTES,
        Some("waiting for decompilation consent"),
      )
      .map { item =>
        if (item == alwaysInWorkspace) {
          tables.dismissedNotifications.DecompilationConsent.dismissForever()
          consentedThisSession.set(true)
          true
        } else if (item == allowThisSession) {
          consentedThisSession.set(true)
          true
        } else false
      }
  }
}

object DecompilationConsent {

  /**
   * Grants consent for the current server session only; the user is prompted
   * again after a restart. The label names the scope explicitly so it reads as
   * a deliberate narrower choice next to [[alwaysInWorkspaceTitle]].
   */
  val allowThisSessionTitle = "Allow for this session"

  /** Grants consent permanently for this workspace, persisted across restarts. */
  val alwaysInWorkspaceTitle = "Always allow in this workspace"

  /**
   * How long to wait for the user to answer the consent prompt before giving
   * up. Generous so a user reading the warning isn't cut off, but bounded so a
   * client that never answers (headless/CI) can't block decompile navigation
   * for the rest of the session.
   */
  private final val promptTimeoutMinutes = 5
}
