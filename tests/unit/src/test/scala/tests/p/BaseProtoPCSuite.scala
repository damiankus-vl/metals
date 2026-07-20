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
  // Java outline; these tests assert the proto declaration only, so they
  // filter to the `.proto` location.
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
}
