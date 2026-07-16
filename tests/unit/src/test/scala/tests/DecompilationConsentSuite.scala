package tests

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import scala.meta.internal.metals.DecompilationConsent
import scala.meta.internal.metals.clients.language.NoopLanguageClient

import org.eclipse.lsp4j.MessageActionItem
import org.eclipse.lsp4j.ShowMessageRequestParams

/**
 * A language client that records how many consent prompts it received and
 * answers each with the same action.
 */
private class CountingConsentClient(answer: String) extends NoopLanguageClient {
  val prompts = new AtomicInteger(0)
  override def showMessageRequest(
      params: ShowMessageRequestParams
  ): CompletableFuture[MessageActionItem] = {
    prompts.incrementAndGet()
    CompletableFuture.completedFuture(new MessageActionItem(answer))
  }
}

class DecompilationConsentSuite extends BaseTablesSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  test("decline-is-remembered-so-a-second-request-does-not-prompt-again") {
    // A single navigation can reach the consent gate twice (the standard
    // decompile path and the decompiled-classpath fallback). Declining must
    // stick for the session so the user sees one prompt, not two.
    val client = new CountingConsentClient("Cancel")
    val consent = new DecompilationConsent(client, tables)

    val first = Await.result(consent.ensureConsent(), 5.seconds)
    val second = Await.result(consent.ensureConsent(), 5.seconds)

    assertEquals(first, false)
    assertEquals(second, false)
    assertEquals(
      client.prompts.get(),
      1,
      "declining consent should prompt at most once per session",
    )
  }

  test("grant-is-remembered-so-a-second-request-does-not-prompt-again") {
    val client =
      new CountingConsentClient(DecompilationConsent.allowThisSessionTitle)
    val consent = new DecompilationConsent(client, tables)

    val first = Await.result(consent.ensureConsent(), 5.seconds)
    val second = Await.result(consent.ensureConsent(), 5.seconds)

    assertEquals(first, true)
    assertEquals(second, true)
    assertEquals(
      client.prompts.get(),
      1,
      "granting consent should prompt at most once per session",
    )
  }
}
