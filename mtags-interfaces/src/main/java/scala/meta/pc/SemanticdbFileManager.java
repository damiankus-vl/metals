package scala.meta.pc;

import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

public interface SemanticdbFileManager {
  default Map<String, Set<Path>> listAllPackages() {
    return Collections.emptyMap();
  }

  /**
   * Text for source path entries that exist only in memory, keyed by the path
   * naming them, for example {@code /w/.metals/readonly/dependencies/
   * proto-generated/a/model.proto/User.java}.
   *
   * These paths also appear in {@link #listAllPackages()}. This only says what
   * to read instead of the file system. A path absent from here is read from
   * disk.
   *
   * <p>The Scala 2 presentation compiler reads this once, when it is built, so
   * it matches the package listing from the same moment. The Java presentation
   * compiler is served the same text through its own file manager.
   */
  default Map<Path, String> inMemorySourceFiles() {
    return Collections.emptyMap();
  }

  public static final SemanticdbFileManager EMPTY = new SemanticdbFileManager() {};
}
