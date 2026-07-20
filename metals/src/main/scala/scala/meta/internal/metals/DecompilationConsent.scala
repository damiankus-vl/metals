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
 * consent, since decompiled output is derived from bytecode and may be
 * restricted by the library's license.
 *
 * Consent is granted per session, not per file or per class: the first
 * answer governs every later decompilation until the server restarts. This
 * matters because a single goto-definition decompiles the class twice (once
 * to find the line to jump to, once to render its contents), and both calls
 * must share the same answer instead of prompting twice.
 */
class DecompilationConsent(
    languageClient: MetalsLanguageClient,
    tables: Tables,
)(implicit ec: ExecutionContext) {

  private val consentedThisSession = new AtomicBoolean(false)

  // The pending or last-settled answer, shared across all callers so both a
  // decline and a grant stick for the session and concurrent callers reuse
  // the same in-flight prompt instead of opening their own. Guarded by
  // `this`. Cleared only on failure/timeout, so an unanswered prompt can be
  // retried later; a grant is instead recorded permanently in
  // `consentedThisSession`.
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
      // An unanswered prompt (dismissed dialog, or a headless client without
      // `window/showMessageRequest`) times out and fails the future; treat
      // that as "not granted" rather than leaving callers hanging.
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

  /** Grants consent for the current server session only; asked again after a restart. */
  val allowThisSessionTitle = "Allow for this session"

  /** Grants consent permanently for this workspace, persisted across restarts. */
  val alwaysInWorkspaceTitle = "Always allow in this workspace"

  /**
   * How long to wait for the user to answer the consent prompt. Generous
   * enough not to cut off someone reading the warning, but bounded so a
   * client that never answers (e.g. headless/CI) doesn't block decompile
   * navigation for the rest of the session.
   */
  private final val promptTimeoutMinutes = 5
}
