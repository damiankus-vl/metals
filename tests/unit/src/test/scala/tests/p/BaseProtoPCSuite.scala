package tests.p

import scala.concurrent.Future

import scala.meta.internal.metals.Configs.ProtobufLspConfig
import scala.meta.internal.metals.Configs.ReferenceProviderConfig
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.UserConfiguration

import org.eclipse.lsp4j.Location
import tests.BaseLspSuite
import tests.BuildInfo

abstract class BaseProtoPCSuite(name: String) extends BaseLspSuite(name) {

  override def userConfig: UserConfiguration =
    super.userConfig.copy(
      fallbackScalaVersion = Some(BuildInfo.scalaVersion),
      presentationCompilerDiagnostics = true,
      buildOnChange = false,
      buildOnFocus = false,
      protobufLspConfig = ProtobufLspConfig.enabled,
      referenceProvider = ReferenceProviderConfig.mbt,
      workspaceSymbolProvider = WorkspaceSymbolProviderConfig.mbt,
    )

  override def initializeGitRepo: Boolean = true

  // Goto-definition on a proto-generated symbol also returns the synthesized
  // Java outline; filter to the `.proto` location to assert only the proto
  // declaration.
  def assertProtoDefinition(
      filename: String,
      query: String,
      expected: String,
  )(implicit loc: munit.Location): Future[List[Location]] =
    server.assertDefinition(
      filename,
      query,
      expected,
      includeLocation = _.getUri().endsWith(".proto"),
    )

  /**
   * Asserts the workspace-relative files the definition resolves to, in the
   * order the client receives them -- the first is where the editor jumps, the
   * rest fill the dropdown.
   *
   * [[tests.TestingServer.assertDefinition]] sorts its messages, so it cannot
   * express ordering.
   */
  def assertDefinitionFileOrder(
      filename: String,
      query: String,
      expected: List[String],
  )(implicit location: munit.Location): Future[Unit] =
    for {
      locations <- server.definitionSubstringQuery(filename, query)
    } yield {
      val workspaceUri = workspace.toURI.toString()
      val files = locations.map(_.getUri().stripPrefix(workspaceUri))
      assertNoDiff(files.mkString("\n"), expected.mkString("\n"))
    }
}
