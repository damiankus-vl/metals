package scala.meta.internal.metals.mbt

import java.nio.file.Files
import java.nio.file.Path
import java.util.Optional
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import java.{util => ju}
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.StandardJavaFileManager

import scala.collection.concurrent.TrieMap
import scala.collection.parallel.mutable.ParArray
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.BatchedFunction
import scala.meta.internal.metals.Configs.TurbineRecompileDelayConfig
import scala.meta.internal.metals.PcQueryContext
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.Sleeper
import scala.meta.pc.ProgressBars

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSet
import com.google.turbine.binder.Binder
import com.google.turbine.binder.ClassPath
import com.google.turbine.binder.ClassPathBinder
import com.google.turbine.binder.JimageClassBinder
import com.google.turbine.binder.Processing
import com.google.turbine.diag.SourceFile
import com.google.turbine.diag.TurbineLog
import com.google.turbine.lower.Lower
import com.google.turbine.parse.Parser
import com.google.turbine.tree.Tree

object TurbineCompiler {

  val emptyResult: TurbineCompileResult = TurbineCompileResult(
    ClassPathBinder.bindClasspath(List.empty.asJava),
    Lower.Lowered.create(ImmutableMap.of(), ImmutableSet.of()),
    DeclaredTypes.empty,
  )

  /**
   * Compiles the inputs, and reports the types they produced under the path of the
   * input they came from.
   *
   * Turbine says which source declared what, so the sources are not read a second
   * time and the names are the ones its output is served under. They cannot be spelled
   * differently from it.
   */
  def compileClassfiles[T](
      toParse: ParArray[T],
      toSourceFile: T => Seq[SourceFile],
      sourcePath: T => String,
      classpath: Seq[Path],
      progressBars: ProgressBars,
  )(implicit rc: ReportContext): TurbineCompileResult = {
    val bar =
      progressBars.start(new ProgressBars.StartProgressBarParams("Outlining"))
    try {
      val parsed = parseInputs(toParse, toSourceFile, sourcePath)
      compileClassfilesInternal(parsed, classpath)
    } catch {
      case NonFatal(e) =>
        PcQueryContext(None, () => classpath.mkString("\n"))
          .report("turbine-file-manager-error", e, "")
        emptyResult
    } finally {
      progressBars.end(bar)
    }
  }

  /**
   * The parsed sources, and the path of the input a source came from.
   *
   * One input can produce several sources. A `.proto` contributes an outline per
   * generated toplevel class, and the types of those outlines go together when it is
   * deleted, so they answer to the proto's path.
   */
  private final case class ParsedInputs(
      compilationUnits: ImmutableList[Tree.CompUnit],
      parsedPathToSourcePath: Map[String, String],
  )

  private def parseInputs[T](
      inputs: ParArray[T],
      toSourceFile: T => Seq[SourceFile],
      sourcePath: T => String,
  ): ParsedInputs = {
    val compilationUnits = new ConcurrentLinkedQueue[Tree.CompUnit]()
    val parsedPathToSourcePath =
      new ju.concurrent.ConcurrentHashMap[String, String]()
    inputs.foreach { input =>
      try {
        // An empty result means the entry doesn't exist or isn't Java-related.
        toSourceFile(input).foreach { source =>
          compilationUnits.add(Parser.parse(source))
          parsedPathToSourcePath.put(source.path(), sourcePath(input))
        }
      } catch {
        case NonFatal(_) =>
        // Silently ignore parse errors, they're very noisy
      }
    }
    // Snapshot rather than hand out `asScala`, which is a view of the mutable map.
    ParsedInputs(
      ImmutableList.copyOf(compilationUnits),
      parsedPathToSourcePath.asScala.toMap,
    )
  }

  private def compileClassfilesInternal(
      parsed: ParsedInputs,
      classpath: Seq[Path],
  ): TurbineCompileResult = {
    val log = new TurbineLog()
    val boundClasspath =
      ClassPathBinder.bindClasspath(validClasspaths(classpath).asJava)
    val result = Binder.bind(
      log,
      parsed.compilationUnits,
      boundClasspath,
      Processing.ProcessorInfo.empty(),
      JimageClassBinder.bindDefault(),
      Optional.empty(),
    )
    // `units` holds a bound class per type the sources declare, nested ones included,
    // and a bound class names the source it was read from.
    //
    // Grouped by the input's path rather than by the parsed source's own path, since
    // several sources can share an input. A `.proto` parses to an outline per generated
    // toplevel class, and grouping by the parsed path would leave the proto holding
    // whichever outline came last.
    val sourcePathToTypes = result
      .units()
      .asScala
      .toSeq
      .flatMap { case (symbol, bound) =>
        parsed.parsedPathToSourcePath
          .get(bound.source().path())
          .map(_ -> symbol.binaryName())
      }
      .groupMap { case (sourcePath, _) => sourcePath } { case (_, typeName) =>
        typeName
      }
      .map { case (sourcePath, typeNames) => sourcePath -> typeNames.toSet }
    val lowered = Lower.lowerAll(
      Lower.LowerOptions.createDefault(),
      result.units(),
      result.modules(),
      result.classPathEnv(),
    )
    TurbineCompileResult(
      boundClasspath,
      lowered,
      DeclaredTypes(sourcePathToTypes),
    )
  }
  private[mbt] def validClasspaths(classpath: Seq[Path]): Seq[Path] = {
    classpath.filter(isJarFile)
  }
  private[mbt] def isJarFile(path: Path): Boolean = {
    Files.isRegularFile(path) &&
    path.getFileName().toString().endsWith(".jar")
  }
}

private case class SourcepathJavaFileObject(
    javaFileObject: JavaFileObject,
    isCompiled: AtomicBoolean = new AtomicBoolean(false),
    isDeleted: AtomicBoolean = new AtomicBoolean(false),
)

class TurbineCompiler[T](
    allCompilationUnits: () => ParArray[T],
    parseUnit: T => Seq[SourceFile],
    /**
     * The path a compilation records an input's types under, and what [[onDidDelete]]
     * looks them up by. An input that parses to several sources records them together
     * under this one path.
     *
     * A path rather than a URI because this runs for every input of every compilation,
     * and building a file URI stats the path to decide on a trailing slash.
     */
    sourcePath: T => String,
    classpath: () => Seq[Path],
    progressBars: ProgressBars,
    turbineRecompileDelay: () => TurbineRecompileDelayConfig,
    listProtoJavaOutlinesForPackage: String => Iterator[JavaFileObject],
    sleeper: Sleeper,
    onIndexingDone: () => Unit,
    onNewProjectClasspath: ClassPath => Unit,
    turbineCache: Option[TurbineCache] = None,
    getDirtyJavaFiles: () => Seq[(String, JavaFileObject)] = () => Seq.empty,
)(implicit ec: ExecutionContext, rc: ReportContext) {
  private val sourcepathByPackageName =
    TrieMap.empty[String, ju.concurrent.ConcurrentLinkedDeque[
      SourcepathJavaFileObject
    ]]
  // Binary names of classes that have been deleted but not yet recompiled.
  // These are excluded from CLASS_PATH listing until the next turbine compile
  // removes them from the compiled output.
  private val deletedBinaryNames = ju.Collections.newSetFromMap(
    new ju.concurrent.ConcurrentHashMap[String, java.lang.Boolean]()
  )

  private def sourcepathSources(): Seq[SourcepathJavaFileObject] = {
    for {
      (_, deque) <- sourcepathByPackageName.iterator
      sourcepathJavaFileObject <- deque.asScala.iterator
    } yield sourcepathJavaFileObject
  }.toSeq
  private def debounceDelay: FiniteDuration = turbineRecompileDelay().duration
  // When delay is >= 1 hour, consider turbine recompilation effectively disabled.
  // In this mode, we rely entirely on SOURCE_PATH fallback for updated sources.
  private def isRecompilationDisabled: Boolean =
    debounceDelay.toMillis >= 3600000

  private val isFirstCompile = new AtomicBoolean(true)
  private val doCompile =
    BatchedFunction.fromFuture[Unit, TurbineCompileResult](
      _ => {
        // If recompilation is disabled, return immediately without waiting.
        // This allows SOURCE_PATH fallback to handle all updates.
        if (isRecompilationDisabled) {
          Future.successful(result)
        } else {
          val toCompile = sourcepathSources()
          for {
            _ <- sleeper.sleep(debounceDelay)
          } yield {
            val result = doCompileNow()
            toCompile.foreach(_.isCompiled.set(true))
            result
          }
        }
      },
      "turbine-compile",
    )
  private def cleanup(): Unit = {
    sourcepathByPackageName.valuesIterator.foreach(
      _.removeIf(_.isCompiled.get())
    )
  }

  @volatile var result = TurbineCompiler.emptyResult

  /**
   * The output a previous session cached, with the types it recorded under their
   * source. Empty when caching is off, when no cache matches the current git hash, or
   * when reading it failed.
   */
  def loadFromCache(classpathPaths: Seq[Path]): Option[TurbineCompileResult] = {
    turbineCache match {
      case Some(cache) =>
        cache.readCache(classpathPaths) match {
          case Some(cached) =>
            scribe.info(
              s"Loaded turbine cache with ${cached.lowered.symbols().size()} symbols"
            )
            Some(cached)
          case None =>
            None
        }
      case None =>
        None
    }
  }

  def doCompileNow(): TurbineCompileResult = {

    def compile() = {
      val compiled = TurbineCompiler.compileClassfiles(
        allCompilationUnits(),
        parseUnit,
        sourcePath,
        classpath(),
        progressBars,
      )
      result = compiled
      cleanup()
      // Clear deleted binary names after recompile - they are no longer in the compiled output
      deletedBinaryNames.clear()
      // Write to cache after successful compilation
      turbineCache.foreach(_.writeCache(compiled))
    }

    if (isFirstCompile.getAndSet(false)) {
      loadFromCache(TurbineCompiler.validClasspaths(classpath())) match {
        case Some(cached) =>
          result = cached
          // Add dirty files to sourcepath so they take precedence over cached classes
          addDirtyFilesToSourcepath()
        case None =>
          compile()
      }
    } else {
      compile()
    }
    onIndexingDone()
    result
  }

  /**
   * Hides from CLASS_PATH the types the last compilation produced for a file, until
   * a recompile rebuilds the output without them, and soft-deletes the file from the
   * sourcepath so SOURCE_PATH stops returning it.
   *
   * Called for a deleted file, and for a changed one. A `.proto` that stopped declaring
   * a message leaves that message's types in the output just as a deletion does, so
   * what gets hidden is what the *previous* content compiled to.
   *
   * @param sourcePath the deleted file, as the compilation recorded its types under
   * @param fileUri the same file as a URI, which is what the sourcepath entries carry
   * @return the types now hidden, empty when no compilation has read this file
   */
  def onDidDelete(sourcePath: String, fileUri: String): Set[String] = {
    val compiledFromFile = result.declaredTypes.forSource(sourcePath)
    compiledFromFile.foreach(deletedBinaryNames.add)
    // Soft-delete from sourcepath so the deleted file isn't returned via SOURCE_PATH
    sourcepathByPackageName.valuesIterator.foreach { deque =>
      deque.asScala.foreach { obj =>
        if (obj.javaFileObject.toUri().toString() == fileUri) {
          obj.isDeleted.set(true)
        }
      }
    }
    compiledFromFile
  }

  /**
   * Check if a binary name has been deleted but not yet recompiled.
   */
  def isDeleted(binaryName: String): Boolean = {
    deletedBinaryNames.contains(binaryName)
  }

  def compileNow(): Future[TurbineCompileResult] = Future {
    doCompileNow()
  }

  def scheduleCompile(): Future[TurbineCompileResult] = {
    doCompile(())
  }

  def onDidChange(
      packageName: String,
      javaFileObject: JavaFileObject,
  ): Future[TurbineCompileResult] = {
    addToSourcepath(packageName, javaFileObject)
    doCompile(())
  }

  /**
   * Add dirty Java files to the sourcepath so they take precedence over cached classes.
   * This is called after loading from cache to ensure uncommitted changes are properly handled.
   */
  private def addDirtyFilesToSourcepath(): Unit = {
    val dirtyFiles = getDirtyJavaFiles()
    if (dirtyFiles.nonEmpty) {
      scribe.info(
        s"turbine: adding ${dirtyFiles.size} dirty files to sourcepath"
      )
      for ((packageName, javaFileObject) <- dirtyFiles) {
        addToSourcepath(packageName, javaFileObject)
      }
    }
  }

  private def addToSourcepath(
      packageName: String,
      javaFileObject: JavaFileObject,
  ): Unit = {
    require(
      !packageName.endsWith("/"),
      s"package name '$packageName' cannot end with '/'. It should be a javac dot-separate package name like 'com.foo'",
    )
    val deque = sourcepathByPackageName.getOrElseUpdate(
      packageName,
      new ju.concurrent.ConcurrentLinkedDeque[SourcepathJavaFileObject](),
    )
    val obj = SourcepathJavaFileObject(javaFileObject)
    deque.addFirst(obj)

    // Clean up duplicate entries in the dequeue for this file.
    deque.removeIf(item =>
      item.ne(obj) &&
        item.javaFileObject.getName() == obj.javaFileObject.getName()
    )
  }

  def createFileManager(
      underlying: StandardJavaFileManager,
      projectClasspathJars: ju.List[Path],
  ): JavaFileManager = {
    val isGlobalClasspathEntry = this.classpath().toSet
    val filteredProjectClasspath =
      projectClasspathJars.asScala.filter(file =>
        !isGlobalClasspathEntry(file) && TurbineCompiler.isJarFile(file)
      )
    val projectClasspath =
      ClassPathBinder.bindClasspath(filteredProjectClasspath.asJava)
    onNewProjectClasspath(projectClasspath)
    new TurbineClasspathFileManager(
      underlying,
      () => result,
      listSourcepath = listCombinedSourcepath,
      isDeleted,
      projectClasspath,
    )
  }

  // Combines normal Scala/Java files with on-the-fly generated Protobuf outlines.
  private def listCombinedSourcepath(
      packageName: String
  ): java.lang.Iterable[JavaFileObject] = {
    val turbineFiles = listSourcepath(packageName)
    val protoPackage = packageName.replace('.', '/') + "/"
    val protoFiles = listProtoJavaOutlinesForPackage(protoPackage)
    if (protoFiles.isEmpty) {
      turbineFiles
    } else {
      val combined = new ju.ArrayList[JavaFileObject]()
      turbineFiles.forEach(combined.add(_))
      protoFiles.foreach(combined.add(_))
      combined
    }
  }

  private[mbt] def listSourcepath(
      packageName: String
  ): java.lang.Iterable[JavaFileObject] = {
    sourcepathByPackageName.get(packageName) match {
      case None =>
        ju.Collections.emptyList()
      case Some(deque) =>
        val isHandledFileName = new ju.HashSet[String]()
        deque.asScala.view
          .flatMap(obj =>
            if (
              // compiled files are loaded via CLASS_PATH
              obj.isCompiled.get() ||
              // soft-deleted files should not be returned
              obj.isDeleted.get() ||
              // if a file is changed multiple times within the same window then
              // we will have multiple entries in the dequeue.
              isHandledFileName.contains(obj.javaFileObject.getName())
            ) {
              None
            } else {
              isHandledFileName.add(obj.javaFileObject.getName())
              Some(obj.javaFileObject)
            }
          )
          .asJava
    }
  }
}
