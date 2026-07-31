package scala.meta.internal.metals.mbt

import java.nio.file.Path

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.mtags.proto.ProtoDeclaration
import scala.meta.internal.mtags.proto.ProtoLayout
import scala.meta.internal.mtags.proto.ProtoNameEvidence
import scala.meta.io.AbsolutePath

/**
 * Which `.proto` a generated Java or Scala file came from, and which files a
 * `.proto` generated.
 *
 * Every consumer asks through here so that one set of rules decides it. The
 * rules are ranked by how much they infer -- see [[ProtoEvidence]] -- and
 * nothing is invented: a symbol under a package some proto configures, whose
 * name matches neither a declaration nor anything derived from the file name,
 * has no origin here.
 *
 * All dependencies are injected as functions rather than taken as an index, so
 * the rules can be exercised without one.
 *
 * @param protoLayoutOf what a `.proto` says about its generated code
 * @param filesInPackage the indexed files declaring a package symbol
 * @param documentOf the index entry for a file
 * @param outlinesOf the Java outlines Metals synthesized from a `.proto`
 * @param textOf a file's contents, for reading a generator's header comment
 * @param allProtoFiles every indexed `.proto`, for resolving a header path
 */
final class ProtoOutputMapping(
    workspace: AbsolutePath,
    protoLayoutOf: AbsolutePath => Option[ProtoLayout],
    filesInPackage: String => Iterator[AbsolutePath],
    documentOf: AbsolutePath => Option[IndexedDocument],
    outlinesOf: AbsolutePath => Seq[VirtualTextDocument],
    textOf: AbsolutePath => Option[String],
    allProtoFiles: () => Iterator[AbsolutePath],
) {

  /**
   * The protos `symbol` could have been generated from, most likely first.
   *
   * Several are possible and all are returned: two protos in one package can
   * both declare a plausible name, and the caller offers the alternatives rather
   * than having one silently chosen for it.
   */
  def originsOfSymbol(symbol: String): Seq[ProtoOrigin] = {
    val querySymbol = Symbol(symbol)
    if (symbol.isEmpty || querySymbol.isPackage || querySymbol.isNone) Nil
    else {
      val origins = for {
        proto <- protosEnclosing(querySymbol)
        layout <- protoLayoutOf(proto)
        nameMatch <- layout.matches(querySymbol)
      } yield ProtoOrigin(
        proto,
        ProtoEvidence.of(nameMatch.evidence),
        nameMatch.declaration,
        generatedSourcesOf(layout, proto, symbol),
      )
      rank(origins)
    }
  }

  /**
   * The protos `file` was generated from, most likely first.
   *
   * A file Metals synthesized names its proto in its own path, and a file a
   * generator wrote may name it in a header comment; both beat inferring
   * anything from the symbols it declares.
   */
  def originsOfFile(file: AbsolutePath): Seq[ProtoOrigin] = {
    val uri = file.toURI.toString()
    val fromPath = ProtoJavaVirtualFile
      .extractProtoPath(uri)
      .orElse(ProtoGeneratedJavaFiles.protoPathFor(workspace, file))
      .map(proto => origin(proto, ProtoEvidence.OutlinePath, file))
    val fromHeader =
      if (fromPath.isDefined) None
      else protoFromHeader(file).map(origin(_, ProtoEvidence.Banner, file))
    fromPath.orElse(fromHeader) match {
      case Some(found) => Seq(found)
      case None => rank(declaredOriginsOfFile(file))
    }
  }

  /**
   * The indexed files a `.proto` generated: those declaring a package it
   * configures, or a sub-package of one named after the file.
   *
   * Only files the index holds, so nothing is reported for output that was
   * cleaned away or lives in a jar -- which is not the same as there being no
   * mapping.
   */
  def outputsOfProto(proto: AbsolutePath): Seq[AbsolutePath] =
    protoLayoutOf(proto).toSeq.flatMap(layout => generatedFiles(layout).toSeq)

  /**
   * The message, enum, or service in `proto` that `symbol` was generated from.
   *
   * Answers the question [[originsOfSymbol]] already answers on the way to
   * picking a proto, for a caller that arrived at the proto some other way -- via
   * an outline's path or a generator's header comment, neither of which says
   * anything about which declaration is meant.
   */
  def declarationOf(
      proto: AbsolutePath,
      symbol: String,
  ): Option[ProtoDeclaration] =
    for {
      layout <- protoLayoutOf(proto)
      nameMatch <- layout.matches(Symbol(symbol))
      declaration <- nameMatch.declaration
    } yield declaration

  /**
   * The outlines Metals synthesized from `proto` that declare `symbol`.
   *
   * Under `java_multiple_files` a proto has one outline per top-level
   * declaration, so several can be relevant at once -- a service is both
   * `Svc.java` and `SvcImplBase.java` -- and all of them are returned. Falling
   * back to the outer class is only right in the single-file layout, where it is
   * the file everything lives in; with multiple files it holds no declarations
   * at all and would answer with an empty class.
   */
  def outlinesDeclaring(
      proto: AbsolutePath,
      symbol: String,
  ): Seq[VirtualTextDocument] = {
    val outlines = outlinesOf(proto)
    val querySymbol = Symbol(symbol)
    val declaring = outlines.filter(outline =>
      outline
        .toplevelSymbols()
        .asScala
        .exists(toplevelSymbol =>
          declaresOrEncloses(Symbol(toplevelSymbol), querySymbol)
        )
    )
    if (declaring.nonEmpty) declaring
    else if (protoLayoutOf(proto).exists(_.javaMultipleFiles)) Nil
    else outlines
  }

  /**
   * Whether the URI points at an outline Metals synthesized from a `.proto`:
   * either the virtual entry the Java compiler resolves against,
   * `file:///a/model.proto.metals-proto-java/User.java`, or the file materialized
   * for the client to open,
   * `.metals/readonly/dependencies/proto-generated/a/model.proto/User.java`.
   *
   * Both forms have to be recognized. The compiler sees the first, the client
   * opens the second, and in SemanticDB they are the same symbols as real
   * generated code -- the URI is all that tells them apart.
   */
  def isSynthesizedOutline(uri: String): Boolean =
    ProtoJavaVirtualFile.isProtoJavaUri(uri) ||
      uri.toAbsolutePathSafe
        .flatMap(ProtoGeneratedJavaFiles.protoPathFor(workspace, _))
        .isDefined

  /**
   * A `.proto` whose path ends with the one a generated header names.
   *
   * The `// source:` path protoc writes is relative to its include root, not to
   * the workspace, so resolving it against the workspace usually misses. Ending
   * an indexed proto's path with it is what actually identifies the file; the
   * workspace-relative reading is tried first because it is exact when it holds.
   */
  private def protoFromHeader(file: AbsolutePath): Option[AbsolutePath] =
    for {
      text <- textOf(file)
      declared <- headerSourcePath(text)
      proto <- Some(workspace.resolve(declared))
        .filter(candidate => candidate.isFile && candidate.isProtoFilename)
        .orElse(protoEndingWith(declared))
    } yield proto

  private def headerSourcePath(header: String): Option[String] =
    header.linesIterator
      .takeWhile(line => line.startsWith("//") || line.startsWith(" //"))
      .collectFirst {
        case s"// source: $source" => source.trim
        case s" // testing-only-source: $source" => source.trim
      }
      .filter(_.nonEmpty)

  private def protoEndingWith(declaredPath: String): Option[AbsolutePath] = {
    val declaredSegments = segmentsOf(declaredPath)
    if (declaredSegments.isEmpty) None
    else
      allProtoFiles()
        .find(proto => endsWithSegments(proto.toNIO, declaredSegments))
        .filter(_ => declaredSegments.nonEmpty)
  }

  private def segmentsOf(path: String): List[String] =
    path.split('/').iterator.filter(_.nonEmpty).toList

  private def endsWithSegments(path: Path, segments: List[String]): Boolean = {
    val pathSegments = path.iterator().asScala.map(_.toString).toList
    pathSegments.lengthCompare(segments.length) >= 0 &&
    pathSegments.takeRight(segments.length) == segments
  }

  /** [[originsOfFile]] by what the file declares, when its path and header say nothing. */
  private def declaredOriginsOfFile(
      file: AbsolutePath
  ): Iterator[ProtoOrigin] =
    for {
      document <- documentOf(file).iterator
      if !document.language.isProtobuf
      toplevelSymbol <- toplevelSymbolsOf(document).iterator
      origin <- originsOfSymbol(toplevelSymbol).iterator
    } yield origin

  private def origin(
      proto: AbsolutePath,
      evidence: ProtoEvidence,
      file: AbsolutePath,
  ): ProtoOrigin =
    ProtoOrigin(
      proto,
      evidence,
      declaration = None,
      generatedSources = Seq(ProtoOutput(file, isExactSymbol = false)),
    )

  /**
   * The indexed files `proto` generated that declare `symbol`, exact matches
   * first.
   *
   * Plural, and the reason [[ProtoOrigin.generatedSources]] is: one proto
   * routinely produces several files declaring one declaration --
   * `java_multiple_files` puts a service in both `SvcGrpc.java` and
   * `SvcImplBase.java` -- so a caller that offers only the first hides answers
   * that are just as real.
   *
   * Public because a proto reached through a path or a header comment carries no
   * symbol with it, and its other outputs still have to be found.
   */
  def outputsDeclaring(
      proto: AbsolutePath,
      symbol: String,
  ): Seq[ProtoOutput] =
    protoLayoutOf(proto).toSeq.flatMap(layout =>
      generatedSourcesOf(layout, proto, symbol)
    )

  /**
   * The indexed generated files declaring `symbol`, exact matches first.
   *
   * A generator that roots its output in a sub-package spells the same message
   * differently from one that nests it in an outer class, so a file matched only
   * by name is still offered -- after every file that declares the symbol as
   * asked for.
   */
  private def generatedSourcesOf(
      layout: ProtoLayout,
      proto: AbsolutePath,
      symbol: String,
  ): Seq[ProtoOutput] = {
    val querySymbol = Symbol(symbol)
    val outputs = for {
      file <- generatedFiles(layout).toSeq
      if file != proto
      document <- documentOf(file).toSeq
      declared = document.symbols.map(_.getSymbol())
      isExact = declared.contains(symbol)
      if isExact || declared.exists(declaredSymbol =>
        declaresOrEncloses(Symbol(declaredSymbol), querySymbol)
      )
    } yield ProtoOutput(file, isExact)
    outputs.sortBy(output => (!output.isExactSymbol, output.file.toString()))
  }

  /**
   * Whether a file declaring `declaredSymbol` answers a request for
   * `querySymbol`.
   *
   * Three relations, because the same declaration is spelled three ways. The
   * symbols are equal. Or the declaration encloses the query, which is the
   * outer-class layout: `Outer#` is where `Outer#Msg#` lives, and a nested
   * message stays inside its parent's file even under `java_multiple_files`. Or
   * one chain ends with the other, which is what a generator rooting its output
   * in a sub-package produces: `model/User#` against `Model#User#`.
   */
  def declaresOrEncloses(
      declaredSymbol: Symbol,
      querySymbol: Symbol,
  ): Boolean =
    declaredSymbol == querySymbol || {
      val declaredChain = ProtoLayout.nameChainAfterPackage(declaredSymbol)
      val queryChain = ProtoLayout.nameChainAfterPackage(querySymbol)
      declaredChain.nonEmpty && queryChain.nonEmpty &&
      (ProtoLayout.nameChainEncloses(declaredChain, queryChain) ||
        ProtoLayout.nameChainsCorrespond(declaredChain, queryChain))
    }

  /** Indexed files under a package the proto configures, sub-packages included. */
  private def generatedFiles(layout: ProtoLayout): Iterator[AbsolutePath] = {
    val packages = layout.configuredPackages.iterator.flatMap(packageSymbol =>
      packageSymbol +: layout.fileDerivedSubPackages(packageSymbol)
    )
    for {
      packageSymbol <- packages
      file <- filesInPackage(packageSymbol)
      if !file.isProtoFilename
    } yield file
  }

  /**
   * The indexed protos whose configured package encloses `symbol`, found by
   * walking its package upwards instead of scanning every proto: one lookup per
   * package level, so a generator's extra sub-package costs one more step.
   */
  private def protosEnclosing(symbol: Symbol): Iterator[AbsolutePath] = {
    val protos = for {
      packageSymbol <- enclosingPackages(symbol)
      candidate <- packageSymbol +: prefixStrippedPackages(packageSymbol)
      proto <- filesInPackage(candidate).filter(_.isProtoFilename)
    } yield proto
    protos.distinct.iterator
  }

  /** `symbol`'s package and each package enclosing that, innermost first. */
  private def enclosingPackages(symbol: Symbol): List[String] = {
    def loop(candidate: Symbol, acc: List[String]): List[String] =
      if (candidate.isNone || candidate.isRootPackage) acc.reverse
      else if (candidate.isEmptyPackage) (candidate.value :: acc).reverse
      else loop(candidate.owner, candidate.value :: acc)
    loop(symbol.enclosingPackage, Nil)
  }

  /**
   * The same package with leading segments dropped: `grpc_shaded/x/y/` also as
   * `x/y/` and `y/`.
   *
   * A workspace that shades the protobuf runtime roots generated code under an
   * extra leading package the `.proto` never mentions, while the index stays
   * keyed by what the proto itself says -- deliberately, so an index key cannot
   * shift with a user setting. Walking the query's package upwards therefore
   * never reaches the document, and dropping leading segments is what closes that
   * gap. Broadening the candidate set is safe because a candidate still has to
   * satisfy [[ProtoLayout.matches]], which accepts only the prefix the workspace
   * actually configured.
   */
  private def prefixStrippedPackages(packageSymbol: String): List[String] = {
    val segments = packageSymbol.split('/').filter(_.nonEmpty).toList
    if (segments.lengthCompare(2) < 0) Nil
    else
      for (dropped <- (1 until segments.length).toList)
        yield ProtoLayout.packageSymbolOf(segments.drop(dropped).mkString("."))
  }

  private def toplevelSymbolsOf(document: IndexedDocument): Seq[String] =
    document.symbols.collect {
      case info if Symbol(info.getSymbol()).isToplevel => info.getSymbol()
    }.toSeq

  private def rank(origins: IterableOnce[ProtoOrigin]): Seq[ProtoOrigin] =
    origins.iterator.toSeq
      .distinctBy(origin => origin.proto)
      .sortBy(origin =>
        (
          origin.evidence.rank,
          if (origin.declaration.isDefined) 0 else 1,
          origin.proto.toString(),
        )
      )
}

/**
 * How much a mapping from generated code back to a `.proto` had to infer, from
 * none at all to a name that only looks right.
 */
sealed abstract class ProtoEvidence(val rank: Int)
object ProtoEvidence {

  /** Metals synthesized the file and put the proto's path in its own. */
  case object OutlinePath extends ProtoEvidence(0)

  /** The generator wrote the proto's path into a header comment. */
  case object Banner extends ProtoEvidence(1)

  /** The symbol names a type the proto declares, under a package it configures. */
  case object DeclaredType extends ProtoEvidence(2)

  /** The symbol names the proto's file, through an outer class or sub-package. */
  case object FileDerivedName extends ProtoEvidence(3)

  def of(evidence: ProtoNameEvidence): ProtoEvidence = evidence match {
    case ProtoNameEvidence.DeclaredType => DeclaredType
    case ProtoNameEvidence.FileDerivedName => FileDerivedName
  }
}

/** A generated file, and whether it declares the symbol exactly as requested. */
final case class ProtoOutput(file: AbsolutePath, isExactSymbol: Boolean)

/**
 * A `.proto` a symbol or file was generated from, with every indexed output of
 * that proto which declares the symbol.
 *
 * Plural because one proto routinely produces several relevant files:
 * `java_multiple_files` splits every top-level declaration out, and a service
 * additionally gets its stub base.
 */
final case class ProtoOrigin(
    proto: AbsolutePath,
    evidence: ProtoEvidence,
    declaration: Option[ProtoDeclaration],
    generatedSources: Seq[ProtoOutput],
)
