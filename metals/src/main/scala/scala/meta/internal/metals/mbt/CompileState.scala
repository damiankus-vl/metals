package scala.meta.internal.metals.mbt

import java.nio.file.Path

/**
 * A turbine compile's result together with the classpath actually bound into
 * it -- unlike [[TurbineCompiler]]'s own `classpath` param, which is what
 * the *next* compile will use. `TurbineCompiler.createFileManager` needs
 * `compiledClasspath`: it filters a jar out of a target's own project
 * classpath assuming `result` already covers it, which only holds once a
 * compile including that jar has run. `classpath()` alone can't tell "not
 * yet compiled" apart from "already compiled".
 *
 * Both fields are read from threads other than the one running
 * `doCompileNow`, so they're kept together in one immutable snapshot rather
 * than as two separately mutable fields, which could otherwise be observed
 * out of sync with each other.
 */
private[mbt] final case class CompileState(
    result: TurbineCompileResult,
    compiledClasspath: Set[Path],
)
