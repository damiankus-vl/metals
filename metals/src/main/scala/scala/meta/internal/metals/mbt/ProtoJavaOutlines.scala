package scala.meta.internal.metals.mbt

import scala.meta.internal.mtags.MD5

/**
 * The Java outlines generated from one `.proto`, with digests of their text.
 *
 * The digests belong to these outlines and to no later version of them, so a
 * consumer that recorded them can tell whether a save changed the outlines. A
 * digest keeps no copy of the text.
 *
 * Computed on the first read, so a consumer of the text alone, like the
 * turbine source path, does not pay for hashing.
 */
final case class ProtoJavaOutlines(documents: Seq[VirtualTextDocument]) {
  lazy val digests: Map[String, String] =
    documents.map(document => digestKey(document) -> digest(document)).toMap

  // The virtual URI,
  // `file:///w/a/model.proto.metals-proto-java/User.java`. Read off the
  // outline. The materialized path would need the workspace.
  private def digestKey(document: VirtualTextDocument): String =
    document.uri().toString()

  private def digest(document: VirtualTextDocument): String =
    MD5.compute(document.text)
}

object ProtoJavaOutlines {
  val empty: ProtoJavaOutlines = ProtoJavaOutlines(Seq.empty)
}
