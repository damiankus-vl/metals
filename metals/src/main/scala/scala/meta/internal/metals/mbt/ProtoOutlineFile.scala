package scala.meta.internal.metals.mbt

import java.nio.file.Path

/**
 * A synthesized outline as a source file.
 *
 * `packageSymbol` is the package the outline declares, `com/example/jproto/`.
 * `file` is where the outline would be written,
 * `.metals/readonly/dependencies/proto-generated/a/model.proto/User.java`. The
 * path does not encode the package. It names the outline even when no file has
 * been written.
 *
 * [[ProtoGeneratedJavaFiles.materialize]] writes one to disk.
 */
final case class ProtoOutlineFile(
    packageSymbol: String,
    file: Path,
    text: String,
)
