package scala.meta.internal.metals.mbt

import java.io.BufferedOutputStream
import java.net.URI
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption
import java.util.Comparator
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong
import java.{util => ju}
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.StandardJavaFileManager

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Random
import scala.util.Using
import scala.util.control.NonFatal

import scala.meta.dialects
import scala.meta.infra.Event
import scala.meta.infra.MonitoringClient
import scala.meta.inputs.Input
import scala.meta.internal.infra.NoopMonitoringClient
import scala.meta.internal.jmbt.Mbt
import scala.meta.internal.jpc.SourceJavaFileObject
import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.BaseFallbackClasspaths
import scala.meta.internal.metals.BaseWorkDoneProgress
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.Configs.JavaSymbolLoaderConfig
import scala.meta.internal.metals.Configs.ProtobufLspConfig
import scala.meta.internal.metals.Configs.TurbineCacheConfig
import scala.meta.internal.metals.Configs.TurbineRecompileDelayConfig
import scala.meta.internal.metals.Configs.WorkspaceSymbolProviderConfig
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.EmptyFallbackClasspaths
import scala.meta.internal.metals.EmptyWorkDoneProgress
import scala.meta.internal.metals.FingerprintedCharSequence
import scala.meta.internal.metals.LoggerReportContext
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.Sleeper
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.Timer
import scala.meta.internal.metals.WorkspaceSymbolQuery
import scala.meta.internal.metals.debug.BuildTargetClasses
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.tokenizers.UnexpectedInputEndException
import scala.meta.io.AbsolutePath
import scala.meta.metals.MetalsLanguageServer
import scala.meta.pc
import scala.meta.pc.JavaFileManagerFactory
import scala.meta.pc.SemanticdbFileManager
import scala.meta.pc.SymbolSearch
import scala.meta.pc.SymbolSearchVisitor

import com.google.turbine.diag.SourceFile
import org.eclipse.{lsp4j => l}

case class MbtWorkspaceSymbolSearchParams(
    query: String,
    buildTargetIdentifier: String,
)

case class MbtPossibleReferencesParams(
    references: collection.Seq[String] = Nil,
    implementations: collection.Seq[String] = Nil,
)

object MbtWorkspaceSymbolProvider {
  def isRelevantPath(file: String): Boolean = {
    file.endsWith(".java") ||
    file.endsWith(".proto") ||
    file.endsWith(".scala")
  }
  def forTesting(): MbtWorkspaceSymbolProvider = {
    val tmp = Files.createTempDirectory("mbt-workspace-symbol-provider")
    tmp.toFile().deleteOnExit()
    new MbtWorkspaceSymbolProvider(AbsolutePath(tmp))
  }
}

class MbtWorkspaceSymbolProvider(
    val workspace: AbsolutePath,
    config: () => WorkspaceSymbolProviderConfig = () =>
      WorkspaceSymbolProviderConfig.mbt,
    buffers: Buffers = Buffers(),
    time: Time = Time.system,
    metrics: MonitoringClient = new NoopMonitoringClient(),
    mtags: () => Mtags = () => Mtags.testingSingleton,
    progress: BaseWorkDoneProgress = EmptyWorkDoneProgress,
    onIndexingDone: () => Unit = () => (),
    javaSymbolLoader: () => JavaSymbolLoaderConfig = () =>
      JavaSymbolLoaderConfig.default,
    fallbackClasspaths: () => BaseFallbackClasspaths = () =>
      EmptyFallbackClasspaths,
    sleeper: Sleeper = Sleeper.TestingSleeper,
    turbineRecompileDelay: () => TurbineRecompileDelayConfig = () =>
      TurbineRecompileDelayConfig.fromConfig(None),
    turbineCacheConfig: () => TurbineCacheConfig = () =>
      TurbineCacheConfig.default,
    indexFilters: List[MbtIndexFilter] = MbtIndexFilter.allFilters,
    protobufLspConfig: () => ProtobufLspConfig = () =>
      ProtobufLspConfig.default,
    metalsOutDir: Option[Path] = None,
    mbtBuild: () => MbtBuild = () => MbtBuild.empty,
)(implicit
    val ec: ExecutionContext = ExecutionContext.Implicits.global,
    val rc: ReportContext = LoggerReportContext,
) extends SemanticdbFileManager
    with JavaFileManagerFactory {

  private def logInfoInProdDebugInTests(message: => String): Unit = {
    if (scala.meta.internal.metals.MetalsServerConfig.isTesting)
      scribe.debug(message)
    else
      scribe.info(message)
  }

  private val indexFile: AbsolutePath = workspace.resolve(".metals/index.mbt")
  private val isIndexing: AtomicBoolean = new AtomicBoolean(false)
  private lazy val protobufWorkspace = new MbtProtobufWorkspaceSymbolProvider(
    buffers,
    protobufLspConfig,
    clearAllProtobufCaches,
  )

  /**
   * The Java outlines synthesized from the given `.proto` file (one per
   * generated top-level class). Empty when the file isn't an indexed proto or
   * proto Java-package indexing is disabled.
   */
  def protoJavaOutlines(file: AbsolutePath): Seq[VirtualTextDocument] =
    documents.get(file).toSeq.flatMap(protobufWorkspace.allJavaOutlines)

  private val turbineCache = new TurbineCache(
    workspace,
    turbineCacheConfig,
    turbineRecompileDelay,
    time,
  )

  /**
   * Returns dirty Java files (uncommitted changes) with their package names.
   * Used to populate the sourcepath when loading from cache so that
   * changed files take precedence over cached compiled classes.
   */
  private def getDirtyJavaFiles(): Seq[(String, JavaFileObject)] = {
    val result = for {
      status <- GitVCS.status(workspace)
      if !status.isDeleted && status.file.isJava
    } yield {
      try {
        // Derive IndexedDocument from current source to handle:
        // 1. Untracked Java files (not yet in documents map)
        // 2. Package relocations (stale metadata in existing document)
        val doc = IndexedDocument.fromFile(
          status.file,
          mtags(),
          buffers,
          dialects.Scala3,
        )
        for {
          input <- toInput(status.file)
          pkg <- doc.semanticdbPackages.headOption
        } yield {
          val packageName = normalizePackageName(pkg)
          val compilationUnit: JavaFileObject =
            doc.toSemanticdbCompilationUnit(input)
          (packageName, compilationUnit)
        }
      } catch {
        case NonFatal(e) =>
          scribe.debug(s"mbt-v2: error indexing dirty file ${status.file}: $e")
          None
      }
    }
    result.flatten.toSeq
  }

  /**
   * The synthesized Java outline that declares `symbol`. A member symbol like
   * `com/example/jproto/User#getName().` is matched on its toplevel class,
   * `User`.
   *
   * Gives navigation a location for a symbol the Scala compiler reported
   * without one. That is what it reports for a class it read as a source-path
   * outline.
   */
  def protoJavaOutlineFor(symbol: Symbol): Option[VirtualTextDocument] = {
    // Called for any symbol no other lookup resolved, `scala/Option#get` as
    // much as `com/example/jproto/User#getName`. The package index narrows it
    // to the protos that could declare the symbol, which for a symbol from an
    // ordinary library is none. Then the toplevel binary name decides.
    val owner = symbol.toplevel.owner
    // A proto with no package generates into the empty package. An outline
    // spells it `""` and both a symbol and the index spell it `_empty_/`.
    val outlinePackage = if (owner.isEmptyPackage) "" else owner.value
    val binaryName = symbol.toplevelBinaryName
    val protosInPackage = for {
      indexed <- documentsByPackage.get(owner.value).toSeq
      path <- indexed.asScala
      protoPath = AbsolutePath(path)
      if protoPath.isProtoFilename
    } yield protoPath
    val outlines = for {
      protoPath <- protosInPackage.iterator
      outline <- protoJavaOutlines(protoPath)
      if outline.pkg == outlinePackage
      if outline
        .toplevelSymbols()
        .asScala
        .exists(Symbol(_).toplevelBinaryName == binaryName)
    } yield outline
    outlines.nextOption()
  }

  private val turbineCompiler: TurbineCompiler[AbsolutePath] =
    new TurbineCompiler[AbsolutePath](
      () => documentsKeys,
      file =>
        if (file.toLanguage.isJava) {
          toInput(file)
            .map(input =>
              new SourceFile(
                file.toNIO.toAbsolutePath.normalize().toString,
                input.text,
              )
            )
            .toList
        } else if (
          file.isProtoFilename &&
          protobufWorkspace.isJavaPackageIndexingEnabled
        ) {
          // Include the Java outlines generated from proto files so that
          // turbine can resolve references to proto-generated classes when it
          // header-compiles the workspace. Without these, a Java method
          // returning a proto-generated class gets its signature erased to
          // java.lang.Object in the compiled classfile, breaking hover and
          // completion in files that use that method.
          for {
            doc <- documents.get(file).toList
            outline <- protobufWorkspace.allJavaOutlines(doc)
          } yield {
            val name = outline.uri()
            val path = java.nio.file.Path.of(name)
            val cleanOutlinePath = path.toAbsolutePath().normalize().toString()
            new SourceFile(cleanOutlinePath, outline.text)
          }
        } else {
          Nil
        },
      () => fallbackClasspaths().javaCompilerClasspath(),
      progress,
      // We don't need to re-compile the workspace super regularly because we can
      // load recently changed files from the sourcepath.
      () => turbineRecompileDelay(),
      listProtoJavaOutlinesForPackage = pkg =>
        protobufWorkspace.listProtoJavaOutlinesForPackage(
          pkg,
          documentsByPackage,
          documents,
        ),
      sleeper = sleeper,
      onIndexingDone = onIndexingDone,
      onNewProjectClasspath = classpath =>
        protobufWorkspace.onNewProjectClasspath(classpath),
      turbineCache = Some(turbineCache),
      getDirtyJavaFiles = getDirtyJavaFiles,
    )

  // NOTE: runs unconditionally even if the user config is not mbt-v2 for usage
  // in MetalsLspService.onUserConfigUpdate
  def recompileTurbineClasspath(): Future[Unit] = {
    turbineCompiler.compileNow().ignoreValue
  }

  def scheduleRecompileTurbineClasspath(): Future[Unit] = {
    turbineCompiler.scheduleCompile().ignoreValue
  }

  def close(): Unit = {}

  /**
   * Clears cached Java outlines for a specific proto file.
   * Called when a proto file is saved and we need to regenerate outlines from buffers.
   *
   * Returns whether the save changed the generated outlines. A `// note` line
   * added to a `.proto` does not change them. The caller rebuilds the Scala
   * compilers when they did, so a false yes buys a rebuild.
   */
  def didSave(path: AbsolutePath): Boolean = {
    if (!path.isProtoFilename) false
    else {
      val served = servedProtoOutlines.get(path)
      documents.get(path).foreach { doc =>
        invalidateCompiledProtoJavaOutlines(path, doc)
        doc.clearProtobufJavaOutlinesCache()
      }
      // No entry means no Scala compiler was handed outlines for this proto,
      // so none of them holds text this save replaces. A proto indexed after a
      // compiler was built also has no entry, and that case is caught by
      // `updateDocumentsKeys` instead, once indexing reaches it.
      val changed = served.exists(_ != outlineDigestsOf(path))
      if (changed) protoOutlineVersionCounter.incrementAndGet()
      changed
    }
  }

  /**
   * Forgets the outlines served for a deleted `.proto`.
   *
   * Returns whether a Scala compiler was handed any. A running compiler holds
   * the text it was built with, so it keeps resolving the generated classes of
   * a proto that is gone until the caller drops it.
   */
  def didDeleteProto(path: AbsolutePath): Boolean = {
    val wasServed =
      path.isProtoFilename && servedProtoOutlines
        .remove(path)
        .exists(_.nonEmpty)
    if (wasServed) protoOutlineVersionCounter.incrementAndGet()
    wasServed
  }

  /**
   * Counts the times the generated outlines changed under a running compiler.
   *
   * A compiler records this before it reads the outlines. A mismatch later
   * means it holds text a proto edit has replaced. Dropping the compilers on
   * the edit itself misses one that is still being built, since it is not in
   * the compiler cache yet to be dropped.
   *
   * Comparing this is one number. Comparing what a compiler holds against the
   * protos would be a digest per outline per request.
   */
  def protoOutlineVersion(): Long = protoOutlineVersionCounter.get()

  /**
   * The version to stamp on a Scala compiler now being built, recording the
   * outlines it is about to be handed as one observation.
   *
   * The version is read first, so a proto edit that lands while the digests
   * are being taken bumps past it and the compiler is dropped unused. That is
   * the safe way round. Recording here rather than from the source path itself
   * keeps the source path free of side effects, so a caller that reads it for
   * a diagnostic cannot advance the record past what a compiler really read.
   */
  def beginServingProtoOutlines(): Long = {
    val version = protoOutlineVersionCounter.get()
    for (protoPath <- protoDocumentsKeys)
      servedProtoOutlines.put(protoPath, outlineDigestsOf(protoPath))
    version
  }

  private val protoOutlineVersionCounter = new AtomicLong(0)

  /**
   * A digest of the outline text last handed to a Scala compiler, keyed by
   * `.proto` and then by outline URI.
   *
   * This is the only record of what a running compiler reads that survives a
   * save. The compiler holds the source path it was built with. The document
   * cache the outlines came from is cleared on save.
   *
   * A digest, not the text. The outline text is much larger than the `.proto`.
   * This map outlives the document cache, so holding text would keep a
   * superseded copy of an outline alive after the save that replaced it.
   */
  private val servedProtoOutlines =
    TrieMap.empty[AbsolutePath, Map[String, String]]

  /**
   * Digests of the outlines the given `.proto` generates. Empty when the proto
   * is not indexed, and when proto Java-package indexing is off, since then no
   * outline reaches a source path.
   *
   * [[ProtoJavaOutlines]] computes these beside the outlines they describe, so
   * the text is hashed once per version of the document rather than once per
   * Scala compiler built.
   */
  private def outlineDigestsOf(protoPath: AbsolutePath): Map[String, String] =
    if (!protobufWorkspace.isJavaPackageIndexingEnabled) Map.empty
    else
      documents
        .get(protoPath)
        .fold(Map.empty[String, String])(protobufWorkspace.javaOutlineDigests)

  /**
   * Excludes proto-generated classes that were compiled into the turbine
   * classpath from a previous version of the given proto file. Javac keeps
   * resolving the current classes from fresh SOURCE_PATH outlines; without
   * this, classes removed from the proto file would remain resolvable from
   * stale classfiles until the next turbine recompile.
   */
  private def invalidateCompiledProtoJavaOutlines(
      file: AbsolutePath,
      doc: IndexedDocument,
  ): Unit = {
    if (javaSymbolLoader().isTurbineClasspath) {
      val binaryNames = doc.cachedJavaOutlines
        .flatMap(_.toplevelSymbols().asScala)
        .map(_.stripSuffix("#").stripSuffix("."))
      if (binaryNames.nonEmpty) {
        turbineCompiler.onDidDelete(binaryNames, file.toURI.toString())
        turbineCompiler.scheduleCompile().ignoreValue
      }
    }
  }
  private def clearAllProtobufCaches(): Unit = {
    documentsKeys.foreach { path =>
      if (path.isProtoFilename) {
        documents.get(path).foreach(_.clearProtobufJavaOutlinesCache())
      }
    }
    // Once the classpath shows a shaded protobuf runtime, the outlines are
    // regenerated naming it, `grpc_shaded.com.google.protobuf.Message` for
    // `com.google.protobuf.Message`. A compiler built before that holds
    // outlines referring to a runtime the classpath does not have. Their own
    // package is unchanged. The prefix rewrites references in the text, not
    // where the generated class lives.
    protoOutlineVersionCounter.incrementAndGet()
  }
  // `documentsKeys` is effectively `documents.keys.par` but without the
  // overhead to copy the keys into a parallel collection at query time.  Make
  // sure to call updateDocumentsKeys() when you add or remove a document.
  @volatile private var documentsKeys = ParArray.empty[AbsolutePath]

  // The `.proto` subset of `documentsKeys`, for the source path a compiler
  // build assembles. Without this it walks the whole index to reach the protos,
  // which are a small part of it. Goto-definition goes through
  // `documentsByPackage` instead, which narrows further.
  @volatile private var protoDocumentsKeys = Seq.empty[AbsolutePath]

  private val isIndexRead = new AtomicBoolean(false)

  // Maps SemanticDB package symbol (for example, "scala/collection/") to all
  // the files that directly belong to that package. This index powers repo-wide
  // -sourcepath imports for JavaPruneCompilerFileManager. It's important to manually
  private val documentsByPackage: TrieMap[String, ConcurrentSkipListSet[Path]] =
    TrieMap.empty[String, ju.concurrent.ConcurrentSkipListSet[Path]]

  private val PathComparator: Comparator[Path] = new Comparator[Path] {
    override def compare(o1: Path, o2: Path): Int = o1.compareTo(o2)
  }

  // The source of truth for what files belong to the workspace, and their attached indexed data.
  // DO NOT update this map directly since have a couple derivative collections.
  // Instead, use the following methods to update the index:
  // - onDidChange(file: AbsolutePath): Unit
  // - onDidDelete(file: AbsolutePath): Unit
  // - onDidChangeSymbols(params: OnDidChangeSymbolsParams): Unit
  private val documents: TrieMap[AbsolutePath, IndexedDocument] =
    readIndex()

  def allFiles(): List[AbsolutePath] = {
    documents.keys.toList
  }

  def onReindex(): IndexingStats = try {
    if (isIndexing.compareAndSet(false, true)) {
      onReindexInternal()
    } else {
      scribe.warn(
        "mbt-v2: already indexing workspace symbols, skipping reindex"
      )
      IndexingStats.empty
    }
  } catch {
    case NonFatal(e) =>
      scribe.error(s"mbt-v2: error reindexing workspace symbols", e)
      IndexingStats.empty
  } finally {
    isIndexing.set(false)
  }

  private def onReindexInternal(): IndexingStats = {
    if (!config().isMBT) {
      scribe.warn(s"mbt-v2: config is not mbt-v2, skipping reindex")
      return IndexingStats.empty
    }

    val timer = new Timer(time)
    // Step 1: list all files in HEAD and include OIDs.
    val gitFiles = GitVCS.lsFilesStage(workspace)
    val uncheckedSources = mbtBuild().getUncheckedSources.asScala.toSeq
    val (genSrcJarStrs, genDirStrs) =
      uncheckedSources.partition(_.endsWith(".srcjar"))
    val genDirs = genDirStrs.map(workspace.resolve)
    val srcJars =
      genSrcJarStrs.map(workspace.resolve).filter { p =>
        val ok = p.exists && p.isFile
        if (!ok)
          scribe.warn(s"mbt-v2: uncheckedSources srcjar does not exist: $p")
        ok
      }
    val files = gitFiles ++ GitVCS.lsFilesFromDirs(genDirs) ++ GitVCS
      .lsFilesFromSrcJars(srcJars, workspace)

    if (files.isEmpty) {
      // A more detailed error message is logged if GitVCS.lsFilesStage fails.
      return IndexingStats.empty
    }

    // Step 2: filter down what files in the git repo are missing results in the
    // index.
    val toIndex = ParArray.fromSpecific(for {
      file <- files
      path = workspace.resolve(file.path)
      candidate = MbtFileCandidate(path)
      if MbtIndexFilter.included(indexFilters, candidate)
      isCached = documents.get(path).exists(_.oid == file.oid)
      if !isCached
    } yield path)
    if (toIndex.nonEmpty) {
      logInfoInProdDebugInTests(s"mbt-v2: indexing ${toIndex.length} files")
    }

    val (task, token) = progress.startProgress(
      message = "Indexing workspace symbols",
      withProgress = true,
      showTimer = true,
      onCancel = None,
    )
    try {
      task.maybeProgress.foreach(_.update(0, toIndex.length))

      val indexedFilesCount = new AtomicInteger()
      // Step 3: The actual indexing, happens in parallel. Treat these as regular
      // didChange events for each individual file.
      toIndex.foreach { file =>
        try {
          onDidChangeInternal(file, updateDocumentKeys = false)
          val count = indexedFilesCount.incrementAndGet()
          if (count % 50 == 0) {
            task.maybeProgress.foreach(_.update(count, toIndex.length))
          }
        } catch {
          case NonFatal(e) =>
            scribe.error(s"mbt-v2: error indexing file ${file}", e)
          case _: StackOverflowError =>
            scribe.error(s"mbt-v2: stack overflow indexing file ${file}")
        }
      }
      updateDocumentsKeys(documents)
    } finally {

      val end = new l.WorkDoneProgressEnd()
      end.setMessage(s"done in $timer")
      progress.endProgress(token)
      onIndexingDone()
    }

    metrics.recordEvent(
      Event.duration("mbt2_index_workspace_symbol_pre_write", timer.elapsed)
    )

    // Step 4: Write the index to disk. It's technically fine to move writing
    // the index to a background job.  Might be worth doing someday.
    writeIndex()

    // Step 5: record metrics.
    // This metric includes everything to create an up-todate index including
    // - File I/O to read the index from disk
    // - Running `git ls-files --stage`
    // - Parsing and indexing all changed files
    // - File I/O to write the index to disk.
    metrics.recordEvent(
      Event.duration("mbt2_index_workspace_symbol", timer.elapsed)
    )
    logInfoInProdDebugInTests(
      f"time: mbt-v2 loaded index for ${documents.size} files in ${timer}"
    )

    IndexingStats(
      files.length,
      toIndex.length,
      backgroundJobs = Future
        .sequence(
          Seq(
            Future {
              // We intentionally exclude `git status` from the metrics because this command
              // can take a very long time to run in large repos and it's not something we
              // can optimize.
              synchronizeWithGitStatus()
            },
            if (javaSymbolLoader().isTurbineClasspath) {
              turbineCompiler.compileNow()
            } else {
              Future.unit
            },
          )
        )
        .ignoreValue,
    )
  }

  private def synchronizeWithGitStatus(): Unit = {
    GitVCS
      .status(workspace)
      .foreach(file => this.onDidChange(file.file))
  }

  def onDidChangeSymbols(
      params: OnDidChangeSymbolsParams
  ): Future[Unit] = {
    val indexedDoc = IndexedDocument.fromOnDidChangeParams(params)
    putDocument(
      params.path,
      indexedDoc,
      updateDocumentKeys = params.updateDocumentKeys,
    )
  }

  /**
   * Update the document keys after batch indexing. Call this after indexing
   *  multiple files with `updateDocumentKeys = false` to prepare the parallel
   *  array for workspace symbol search.
   */
  def updateDocumentKeys(): Unit = {
    updateDocumentsKeys(documents)
  }

  def onDidDelete(file: AbsolutePath): Future[Unit] = {
    documents.remove(file) match {
      case None => Future.unit
      case Some(doc) =>
        updateDocumentsKeys(documents)
        // Remove from package index
        for {
          pkg <- doc.semanticdbPackages
          files <- documentsByPackage.get(pkg)
        } {
          files.remove(file.toNIO)
        }
        // If Java file, treat deletion as a change to an empty file.
        // This adds an empty source to SOURCE_PATH so javac won't find the class.
        // We also track deleted binary names to exclude from CLASS_PATH.
        if (doc.language.isJava && javaSymbolLoader().isTurbineClasspath) {
          val binaryNames = doc.symbols
            .map(_.getSymbol())
            .filter(sym => Symbol(sym).isToplevel)
            .map(sym => sym.stripSuffix("#").stripSuffix("."))
            .toSeq
          // Track deleted binary names for CLASS_PATH exclusion
          turbineCompiler.onDidDelete(binaryNames, file.toURI.toString())
          // Add empty file to SOURCE_PATH so javac parses it and doesn't find the class
          doc.semanticdbPackages.headOption match {
            case Some(pkg) =>
              val packageName = normalizePackageName(pkg)
              val emptyCompilationUnit = VirtualTextDocument(
                SourceJavaFileObject.makeRelativeURI(file.toURI),
                pc.Language.JAVA,
                "", // Empty content - class is no longer defined
                doc.semanticdbPackages,
                Nil, // No toplevel symbols
              )
              turbineCompiler
                .onDidChange(packageName, emptyCompilationUnit)
                .map(_ => ())
            case None =>
              Future.unit
          }
        } else if (doc.language.isProtobuf) {
          invalidateCompiledProtoJavaOutlines(file, doc)
          Future.unit
        } else {
          Future.unit
        }
    }
  }
  def onDidChange(file: AbsolutePath): Future[Unit] = {
    onDidChangeInternal(file, updateDocumentKeys = true)
  }

  private def onDidChangeInternal(
      file: AbsolutePath,
      updateDocumentKeys: Boolean,
  ): Future[Unit] = try {
    if (MbtIndexFilter.included(indexFilters, MbtFileCandidate(file))) {
      val enableProtoJavaPackage =
        file.isProtoFilename && protobufWorkspace.isJavaPackageIndexingEnabled
      val mdoc =
        IndexedDocument.fromFile(
          file,
          mtags(),
          buffers,
          dialects.Scala3,
          enableProtoJavaPackage = enableProtoJavaPackage,
        )
      putDocument(file, mdoc, updateDocumentKeys = updateDocumentKeys)
    } else Future.unit
  } catch {
    case _: NoSuchFileException =>
      onDidDelete(file)
      Future.unit
    case _: UnexpectedInputEndException =>
      scribe.debug(s"${file}: syntax error")
      Future.unit
    case NonFatal(e) =>
      scribe.error(s"Error indexing file $file", e)
      Future.unit
  }

  private def toInput(
      file: AbsolutePath
  ): Option[Input.VirtualFile] = try {
    Some(file.toInputFromBuffers(buffers))
  } catch {
    case _: java.nio.file.NoSuchFileException =>
      onDidDelete(file)
      None
  }

  override def createFileManager(
      standardFileManager: StandardJavaFileManager,
      classpath: ju.List[Path],
  ): JavaFileManager = {
    if (javaSymbolLoader().isJavacSourcepath) {
      new JavacSourcepathFileManager(
        standardFileManager,
        (pkg) => {
          documentsByPackage.get(pkg) match {
            case None =>
              scribe.debug(s"mbt-v2: package not found in index: $pkg")
              ju.Collections.emptyList()
            case Some(paths) =>
              scribe.debug(
                s"mbt-v2: found ${paths.size()} files for package: $pkg"
              )
              val result = for {
                path <- paths.asScala.iterator
                doc <- documents.get(AbsolutePath(path)).toList.iterator
                compilationUnit <- {
                  if (doc.language.isJava) {
                    toInput(doc.file)
                      .map(doc.toSemanticdbCompilationUnit)
                      .iterator
                  } else if (doc.language.isProtobuf) {
                    protobufWorkspace.generateProtoJavaOutlines(doc, pkg)
                  } else {
                    Iterator.empty
                  }
                }
              } yield compilationUnit
              ArrayBuffer.from(result).asJava
          }
        },
      )
    } else if (javaSymbolLoader().isTurbineClasspath) {
      turbineCompiler.createFileManager(standardFileManager, classpath)
    } else {
      throw new IllegalArgumentException(
        s"unexpected javaSymbolLoader config: ${javaSymbolLoader()}"
      )
    }
  }

  override def listAllPackages(): ju.Map[String, ju.Set[Path]] = {
    val result = new ju.HashMap[String, ju.Set[Path]]()
    // The live set is wrapped, not copied. Copying here would copy the whole
    // index, and this runs on each presentation-compiler build.
    for ((packageName, indexed) <- documentsByPackage)
      result.put(packageName, ju.Collections.unmodifiableSet(indexed))
    // Only a package an outline declares is rebuilt, and a package the index
    // does not know at all is added.
    val packageToOutlines = protoJavaOutlineFiles().groupBy(_.packageSymbol)
    for ((packageName, outlines) <- packageToOutlines) {
      val merged = new ju.HashSet[Path](
        result.getOrDefault(packageName, ju.Collections.emptySet())
      )
      for (outline <- outlines) merged.add(outline.file)
      result.put(packageName, ju.Collections.unmodifiableSet(merged))
    }
    result
  }

  /**
   * The proto Java outlines, for a Scala 2 target's presentation-compiler
   * source path. Nothing is written. The compiler is handed the text through
   * [[inMemorySourceFiles]].
   *
   * Pruned source-path mode keeps an indexed file only when the source path
   * names it too, so an outline has to appear here and in [[listAllPackages]].
   */
  def protoJavaOutlineSourcePaths(): Seq[Path] =
    protoJavaOutlineFiles().map(_.file)

  /**
   * The proto Java outlines, for a Scala 3 target's presentation-compiler
   * source path, written to disk first.
   *
   * Scala 3 resolves a source path entry with `AbstractFile.getFile`, and that
   * answers null for a path with no file on it. It cannot be handed
   * [[inMemorySourceFiles]] the way Scala 2 is.
   */
  def materializedProtoJavaOutlineSourcePaths(): Seq[Path] = {
    val outlines = protoJavaOutlineFiles()
    outlines.foreach(ProtoGeneratedJavaFiles.materialize)
    outlines.map(_.file)
  }

  /**
   * The synthesized outlines, so a Scala compiler can read them without a file
   * on disk. Keyed by the same paths [[protoJavaOutlineSourcePaths]] puts on
   * the source path. That is how the compiler recognizes them.
   */
  override def inMemorySourceFiles(): ju.Map[Path, String] =
    protoJavaOutlineFiles()
      .map(outline => outline.file -> outline.text)
      .toMap
      .asJava

  /**
   * The Java outlines the workspace's protos generate, as source files.
   *
   * The Scala counterpart to [[TurbineCompiler.listCombinedSourcepath]]. The
   * index maps a package to `model.proto` itself. Scalac cannot parse a
   * `.proto`.
   *
   * A path names an outline whether or not a file is on it. Scala 2 is handed
   * the text through [[inMemorySourceFiles]] and writes nothing until
   * navigation sends the client to a file. Scala 3 cannot read a source that
   * way, so [[materializedProtoJavaOutlineSourcePaths]] writes them for it.
   */
  private def protoJavaOutlineFiles(): Seq[ProtoOutlineFile] =
    protoDocumentsKeys.flatMap(protoJavaOutlineFilesOf)

  /** [[protoJavaOutlineFiles]] for a single `.proto`. */
  private def protoJavaOutlineFilesOf(
      protoPath: AbsolutePath
  ): Seq[ProtoOutlineFile] =
    if (!protobufWorkspace.isJavaPackageIndexingEnabled) Nil
    else
      for {
        document <- documents.get(protoPath).toSeq
        outline <- protobufWorkspace.allJavaOutlines(document)
        className <- ProtoJavaVirtualFile
          .extractClassName(outline.uri().toString())
          .toSeq
        javaFile <- ProtoGeneratedJavaFiles
          .pathFor(workspace, protoPath, className)
          .toSeq
      } yield ProtoOutlineFile(
        packageSymbol = outline.pkg,
        file = javaFile.toNIO,
        text = outline.text,
      )

  def document(file: AbsolutePath): Option[IndexedDocument] = {
    documents.get(file)
  }

  def definition(symbol: String): List[l.Location] = {
    val result = (for {
      file <- documentsByPackage
        .getOrElse(
          Symbol(symbol).enclosingPackage.value,
          new ju.concurrent.ConcurrentSkipListSet[Path](),
        )
        .asScala
        .iterator
      doc <- documents.get(AbsolutePath(file)).iterator
      sym <- doc.symbols.iterator
      if sym.getSymbol() == symbol
    } yield {
      new l.Location(
        file.toUri().toString(),
        new l.Range(
          new l.Position(
            sym.getDefinitionRange().getStartLine(),
            sym.getDefinitionRange().getStartCharacter(),
          ),
          new l.Position(
            sym.getDefinitionRange().getEndLine(),
            sym.getDefinitionRange().getEndCharacter(),
          ),
        ),
      )
    }).toList
    result
  }

  /**
   * Finds the proto RPC definition for a gRPC stub method symbol.
   *
   * Maps from Java gRPC method symbol to proto RPC definition.
   * e.g., "com/example/api/jproto/GreeterGrpc$GreeterImplBase#sayHello()."
   *   -> rpc SayHello in greeter.proto
   *
   * This is used for goto-super functionality when a Java class overrides
   * a gRPC ImplBase method.
   */
  def findProtoRpcDefinition(methodSymbol: String): List[l.Location] = {
    protobufWorkspace.findProtoRpcDefinition(methodSymbol, documents)
  }

  /**
   * Extracts potential main class candidates from the MBT index without loading semanticdb.
   * Returns candidates that need to be confirmed via semanticdb before use.
   *
   * Candidates are identified by:
   * 1. Symbols ending in "#main()." or ".main()." (Java/Scala main methods)
   * 2. Files referencing "scala/main#" (@main annotation)
   * 3. Files referencing "scala/App#" (App trait extension)
   */
  def candidateMainClasses(
      filterPath: AbsolutePath => Boolean
  ): Seq[BuildTargetClasses.MainClassCandidate] = {
    val candidates =
      scala.collection.mutable.ArrayBuffer
        .empty[BuildTargetClasses.MainClassCandidate]
    val javaMain = "#main()."
    val scalaMain = ".main()."
    val mainAnnotRef = FingerprintedCharSequence.fuzzyReference("scala/main#")
    val appRef = FingerprintedCharSequence.fuzzyReference("scala/App#")
    val pathsFromMainSymbols =
      queryWorkspaceSymbol("main")
        .flatMap(info => Option(info.getLocation()))
        .map(_.getUri.toAbsolutePath)
    val pathsFromReferences =
      possibleReferences(
        MbtPossibleReferencesParams(
          references = Seq(mainAnnotRef.value.toString, appRef.value.toString)
        )
      )
    for {
      path <- pathsFromMainSymbols.distinct
      if filterPath(path)
      doc <- documents.get(path).toList
    } {

      // Check for main method symbols in the document
      for (symbolInfo <- doc.symbols) {
        val symbol = symbolInfo.getSymbol()
        // Java main method pattern: com/example/Main#main().
        if (symbol.endsWith(javaMain)) {
          val classSymbol = symbol.stripSuffix("main().")
          candidates += BuildTargetClasses.MainClassCandidate(path, classSymbol)
        }
        // Scala main method pattern: com/example/Main.main().
        else if (symbol.endsWith(scalaMain)) {
          val objectSymbol = symbol.stripSuffix("main().")
          candidates += BuildTargetClasses.MainClassCandidate(
            path,
            objectSymbol,
          )
        }
      }
    }

    for {
      path <- pathsFromReferences
      if filterPath(path)
      doc <- documents.get(path).toList
    } {
      // Check bloom filter for @main annotation reference
      if (doc.bloomFilter.mightContain(mainAnnotRef)) {
        // For @main annotated methods, we need to find method symbols
        // that could be annotated. We'll add all method symbols as candidates.
        for (symbolInfo <- doc.symbols) {
          val symbol = symbolInfo.getSymbol()
          val kind = symbolInfo.getKind()
          // Methods that could have @main annotation
          if (kind == Semanticdb.SymbolInformation.Kind.METHOD) {
            candidates += BuildTargetClasses.MainClassCandidate(path, symbol)
          }
        }
      }
      // For App extension, we add class/object symbols as candidates
      for (symbolInfo <- doc.symbols) {
        val symbol = symbolInfo.getSymbol()
        val kind = symbolInfo.getKind()
        if (
          kind == Semanticdb.SymbolInformation.Kind.OBJECT &&
          Symbol(symbol).isToplevel
        ) {
          candidates += BuildTargetClasses.MainClassCandidate(path, symbol)
        }

      }
    }
    candidates.toSeq
  }

  /**
   * BFS through the inheritance chain starting from the given symbols.
   * Returns all files that transitively reference those symbols as parents.
   *
   * At each level, top-level traits and classes defined in the current
   * frontier files are extracted from the MBT index and used as seeds for
   * the next [[possibleReferences]] call. Already-visited paths are excluded
   * to prevent cycles.
   */
  private def transitiveReferenceFiles(
      references: Seq[String],
      implementations: Seq[String],
  ): Set[AbsolutePath] = {
    val allMatchedPaths = scala.collection.mutable.HashSet.empty[AbsolutePath]
    var frontier: Set[AbsolutePath] = possibleReferences(
      MbtPossibleReferencesParams(
        references = references,
        implementations = implementations,
      )
    )

    while (frontier.nonEmpty) {
      allMatchedPaths ++= frontier

      val nextBaseSymbols = frontier.flatMap { path =>
        documents.get(path).toSeq.flatMap { doc =>
          doc.symbols
            .filter { sym =>
              val kind = sym.getKind()
              (kind == Semanticdb.SymbolInformation.Kind.TRAIT ||
                kind == Semanticdb.SymbolInformation.Kind.CLASS) &&
              Symbol(sym.getSymbol()).isToplevel
            }
            .map(_.getSymbol())
        }
      }.toSeq

      frontier =
        if (nextBaseSymbols.isEmpty) Set.empty
        else
          possibleReferences(
            MbtPossibleReferencesParams(implementations = nextBaseSymbols)
          ) -- allMatchedPaths
    }

    allMatchedPaths.toSet
  }

  /**
   * Extracts potential test class candidates from the MBT index without loading semanticdb.
   * Returns candidates that need to be confirmed via semanticdb before use.
   *
   * Candidates are identified by:
   * 1. Files referencing JUnit/TestNG annotation symbols (e.g., "org/junit/Test#")
   * 2. Files referencing base parent classes of test frameworks (e.g., "munit/FunSuite#")
   *
   * @param filterPath A function to filter which paths should be included
   * @param annotationSymbols JUnit/TestNG annotation symbols to search for
   * @param baseParentSymbols Base parent class symbols for ScalaTest, MUnit, Weaver, ZIO Test
   */
  def candidateTestClasses(
      filterPath: AbsolutePath => Boolean,
      annotationSymbols: Seq[String],
      baseParentSymbols: Seq[String],
  ): Seq[BuildTargetClasses.TestClassCandidate] = {
    val candidates =
      scala.collection.mutable.ArrayBuffer
        .empty[BuildTargetClasses.TestClassCandidate]

    val allMatchedPaths = transitiveReferenceFiles(
      references = annotationSymbols,
      implementations = baseParentSymbols,
    )

    for {
      path <- allMatchedPaths
      if filterPath(path)
      doc <- documents.get(path).toList
    } {
      // Add all class/object symbols as potential test class candidates
      for (symbolInfo <- doc.symbols) {
        val symbol = symbolInfo.getSymbol()
        val kind = symbolInfo.getKind()
        // Classes and objects that could be test suites
        if (
          kind == Semanticdb.SymbolInformation.Kind.CLASS ||
          kind == Semanticdb.SymbolInformation.Kind.OBJECT
        ) {
          if (Symbol(symbol).isToplevel) {
            candidates += BuildTargetClasses.TestClassCandidate(path, symbol)
          }
        }
      }
    }
    candidates.toSeq
  }

  def possibleReferences(
      params: MbtPossibleReferencesParams
  ): Set[AbsolutePath] = {
    val queries = HashSet.empty[String]
    params.implementations.foreach { symbol =>
      val sym = Symbol(symbol)
      if (sym.isMethod) {
        queries += s"${sym.displayName}():"
      } else if (sym.isType) {
        queries += s"${sym.displayName}:"
        // this is needed for now because the Scala top-level mtags indexer does not emit ':'
        queries += s"${sym.displayName}#"
      } else if (sym.isTerm) {
        queries += s"${sym.displayName}."
        // Scala vals and vars can be implemented via getters and setters
        queries += s"${sym.displayName}():"
      } else {
        scribe.warn(
          s"mbt-v2: unexpected implementation symbol for possibleReferences: ${symbol}"
        )
      }
    }
    params.references.foreach { ref =>
      val sym = Symbol(ref)
      if (sym.isGlobal) {
        if (sym.isConstructor) {
          queries += s"${sym.owner.displayName}."
        } else if (sym.isMethod) {
          queries += s"${sym.displayName}()."
        } else {
          queries += s"${sym.displayName}."
          queries += s"${sym.displayName}:"
          // Also search for method references - Java accesses Scala vals as methods
          queries += s"${sym.displayName}()."
        }
      }
    }
    val fingerprints =
      queries.iterator.map(FingerprintedCharSequence.fuzzyReference).toBuffer
    val result = TrieMap.empty[AbsolutePath, Unit]
    for {
      path <- documentsKeys.toList
      doc <- documents.get(path).toList.iterator
      if fingerprints.exists(query => doc.bloomFilter.mightContain(query))
    } {
      if (doc.bloomFilter.isFull) {
        scribe.warn(s"mbt-v2: bloom filter is full for ${path}")
      }
      if (path.exists) {
        result(path) = ()
      } else {
        // Clean up removed files
        documents.remove(path)
      }
    }
    result.keysIterator.toSet
  }

  // Convenience method to avoid dealing with the visitor-based query API
  // (including concurrency).  Mostly useful for testing. In production, use the
  // visitor-based API.
  final def queryWorkspaceSymbol(
      query: String
  ): List[l.SymbolInformation] = {
    val visitor = new SimpleCollectingSymbolSearchVisitor()
    workspaceSymbolSearch(
      new MbtWorkspaceSymbolSearchParams(query, ""),
      visitor,
    )
    visitor.results.asScala.toList
  }

  def workspaceSymbolSearch(
      params: MbtWorkspaceSymbolSearchParams,
      visitor: SymbolSearchVisitor,
  ): SymbolSearch.Result = {
    if (!config().isMBT) {
      scribe.warn(
        s"mbt-v2: config is not mbt-v2, skipping workspace symbol search"
      )
      return SymbolSearch.Result.COMPLETE
    }

    if (params.buildTargetIdentifier.nonEmpty) {
      throw new UnsupportedOperationException(
        s"mbt-v2: build target identifier is not supported yet. Got: '${params.buildTargetIdentifier}'"
      )
    }

    val maxResults = 300
    val fuzzyQuery = WorkspaceSymbolQuery.fuzzy(params.query)
    val exactQuery = WorkspaceSymbolQuery.exactDescriptorPart(params.query)
    val resultCount = new AtomicInteger(0)
    val remainingFilesCount = new AtomicInteger(documentsKeys.length)

    // Step 1: filter out what files are *likely* to contain a match, per bloom
    // filter tests.
    val exactMatches =
      new ju.concurrent.ConcurrentLinkedQueue[IndexedDocument]()
    val fuzzyMatches =
      new ju.concurrent.ConcurrentLinkedQueue[IndexedDocument]()
    for {
      path <- documentsKeys
      if !visitor.isCancelled() && resultCount.get() < maxResults
      _ = remainingFilesCount.decrementAndGet()
      doc <- documents.get(path).toList.iterator
      if fuzzyQuery.matches(doc.bloomFilter)
    } {
      if (exactQuery.matches(doc.bloomFilter)) {
        exactMatches.add(doc)
      } else {
        fuzzyMatches.add(doc)
      }
    }

    // Step 2: brute-force fuzzy search through all the symbols in the documents
    // with potential matches.
    val candidates = ParArray.fromSpecific(
      Iterator(
        exactMatches.asScala.iterator,
        fuzzyMatches.asScala.iterator,
      ).flatten
    )
    for {
      doc <- candidates
      if !visitor.isCancelled() && resultCount.get() < maxResults
      info <- doc.symbols
      if fuzzyQuery.matches(info.getSymbol())
    } {
      resultCount.addAndGet(
        visitor.visitWorkspaceSymbol(
          doc.file.toNIO,
          info.getSymbol,
          info.getKind.toLsp,
          info.getDefinitionRange().toLspRange,
        )
      )
    }

    if (remainingFilesCount.get() > 0) {
      SymbolSearch.Result.INCOMPLETE
    } else {
      SymbolSearch.Result.COMPLETE
    }
  }

  private def putDocument(
      file: AbsolutePath,
      doc: IndexedDocument,
      updateDocumentKeys: Boolean,
  ): Future[Unit] = {
    // .metals/out contains JDK sources materialized for --patch-module; they are not workspace sources
    if (metalsOutDir.exists(outDir => file.toNIO.startsWith(outDir))) {
      Future.unit
    } else if (MbtIndexFilter.included(indexFilters, MbtFileCandidate(file))) {
      val old = documents.put(file, doc)
      if (old == None && updateDocumentKeys) {
        updateDocumentsKeys(documents)
      }
      addDocumentToPackages(doc.semanticdbPackages, file)

      if (
        updateDocumentKeys &&
        doc.language.isJava &&
        javaSymbolLoader().isTurbineClasspath
      ) {
        doc.semanticdbPackages.headOption match {
          case Some(pkg) =>
            val input = file.toInputFromBuffers(buffers)
            val packageName = normalizePackageName(pkg)
            val compilationUnit = doc.toSemanticdbCompilationUnit(input)
            turbineCompiler
              .onDidChange(packageName, compilationUnit)
              .ignoreValue
          case None =>
            Future.unit
        }
      } else if (
        updateDocumentKeys &&
        doc.language.isProtobuf &&
        javaSymbolLoader().isTurbineClasspath
      ) {
        // Covers proto files changed outside the editor (e.g. git checkout);
        // for editor saves, didSave already invalidated before the outline
        // cache was cleared.
        old.foreach(invalidateCompiledProtoJavaOutlines(file, _))
        Future.unit
      } else {
        Future.unit
      }
    } else Future.unit
  }

  private def addDocumentToPackages(
      pkgs: Seq[String],
      file: AbsolutePath,
  ): Unit = {
    val nioFile = file.toNIO
    for (pkg <- pkgs) {
      val files = documentsByPackage.getOrElseUpdate(
        pkg,
        new ConcurrentSkipListSet[Path](PathComparator),
      )
      files.add(nioFile)
    }
  }

  // Dumps the current in-memory index to .metals/index.mbt. This overwrites the
  // old index, which effectively works like basic garbage collection. We don't
  // need 100% cache hits so it's fine to re-index all the changed files every
  // time you checkout between two git commits. We just want to avoid 1) re-indexing
  // 100k files on startup and 2) having an index that grows unbounded.
  private def writeIndex(): Unit = try {
    indexFile.parent.createDirectories()
    val tmp = workspace
      // We create the tmp file under .metals/ because atomic moves can fail on
      // Linux if /tmp is on a different mount than the workspace.
      .resolve(Directories.outDir)
      // Use a random number there are multiple Metals servers indexing the same
      // workspace at the same time, which should be rare, but can happen.
      .resolve(s"index.mbt.${new Random().nextInt()}.tmp")
    tmp.deleteIfExists()
    tmp.parent.createDirectories()
    val bufferedOutputStream = new BufferedOutputStream(
      Files.newOutputStream(
        tmp.toNIO,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE,
      )
    )
    tmp.toFile.deleteOnExit()
    Using(bufferedOutputStream) { out =>
      documents.foreach { case (path, doc) =>
        if (!path.exists) {
          this.documents.remove(path)
        } else {
          // Append one document at a time to the output stream to avoid holding
          // a full copy of the binary payload in memory.  This is the main
          // reason why index.mbt uses protobuf instead of JSON, it's not
          // because Protobuf is super fast or super compact.
          doc.toIndexProto().writeTo(out)
        }
      }
    }
    try {
      Files.move(tmp.toNIO, indexFile.toNIO, StandardCopyOption.ATOMIC_MOVE)
    } catch {
      case NonFatal(e) =>
        scribe.warn(
          s"mbt-v2: failed to move '${tmp}' to '${indexFile}' atomically, trying non-atomic move.",
          e,
        )
        // Fallback to non-atomic move.
        Files.move(tmp.toNIO, indexFile.toNIO)
    }
  } catch {
    case NonFatal(e) =>
      scribe.error(s"mbt-v2:Error writing index file ${indexFile}", e)
  }

  private def updateDocumentsKeys(
      documentsIndex: TrieMap[AbsolutePath, IndexedDocument]
  ): ParArray[AbsolutePath] = {
    val newValue = ParArray.fromSpecific(documentsIndex.keysIterator)
    documentsKeys = newValue
    val newProtoKeys =
      documentsIndex.keysIterator.filter(_.isProtoFilename).toSeq
    // A compiler holds the source path it was built with, so it does not learn
    // of a proto the index gained or lost since. Startup is where this shows.
    // `onReindex` runs beside `onInitialized`, so a compiler built before the
    // index reached the protos would hold a source path without them.
    // `didSave` cannot report it, having no digests to compare for a proto that
    // was not served.
    if (newProtoKeys.toSet != protoDocumentsKeys.toSet)
      protoOutlineVersionCounter.incrementAndGet()
    protoDocumentsKeys = newProtoKeys
    newValue
  }

  private def normalizePackageName(packageName: String): String = {
    packageName.stripSuffix("/").replace("/", ".")
  }

  // Reads .metals/index.mbt, which is a serialized Mbt.Index protobuf payload,
  // into memory and converts it into TrieMap[AbsolutePath, IndexedDocument].
  // For a very large repo (>100k Scala/Java files), this file still only takes
  // ~500mb of ram.
  private def readIndex(): TrieMap[AbsolutePath, IndexedDocument] = try {
    val result = TrieMap.empty[AbsolutePath, IndexedDocument]
    val timer = new Timer(time)
    if (indexFile.exists) {
      val index = Mbt.Index.parseFrom(indexFile.readAllBytes)
      for {
        doc <- index.getDocumentsList().asScala.iterator
        // Don't load old and incompatible versions of indexed files
        if IndexedDocument.matchesCurrentVersion(doc)
      } {
        try {
          val path = AbsolutePath.fromAbsoluteUri(URI.create(doc.getUri()))
          addDocumentToPackages(
            doc.getSemanticdbPackageList().asScala.toList,
            path,
          )
          result.put(path, IndexedDocument.fromProto(path, doc))
        } catch {
          case NonFatal(e) =>
            scribe.error(s"Error reading index file ${doc.getUri()}", e)
        }
      }
      logInfoInProdDebugInTests(
        s"mbt-v2: read index for ${result.size} files in ${timer}"
      )
    }
    updateDocumentsKeys(result)
    val indexDocumentsCount = documentsKeys.length.toString()
    metrics.recordEvent(
      Event
        .duration("mbt2_index_workspace_symbol_read_index", timer.elapsed)
        .withLabel("index_documents_count", indexDocumentsCount)
    )
    if (isIndexRead.compareAndSet(false, true)) {
      // This metric can be used as a "time to first intelligence" metric to
      // measure how long it takes for Metals to start up and provide meaningful
      // diagnostics/definitions.
      metrics.recordEvent(
        Event
          .duration(
            "mbt2_index_workspace_symbol_read_index_since_start",
            MetalsLanguageServer.durationSinceStart(),
          )
          .withLabel("index_documents_count", indexDocumentsCount)
      )
      MetalsLanguageServer
        .durationSinceExtensionStart()
        .foreach { extensionStartDuration =>
          metrics.recordEvent(
            Event
              .duration(
                "mbt2_index_workspace_symbol_read_index_since_extension_start",
                extensionStartDuration,
              )
              .withLabel("index_documents_count", indexDocumentsCount)
          )
        }
    }
    result
  } catch {
    case NonFatal(e) =>
      scribe.error(s"Error reading repo-wide symbol index at '${indexFile}'", e)

      TrieMap.empty[AbsolutePath, IndexedDocument]
  }

}
