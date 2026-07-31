package scala.meta.internal.mtags.proto

import java.{util => ju}

/**
 * The naming rules protobuf code generators share, in one place.
 *
 * Both directions live here on purpose. A generated accessor name is derived
 * from a proto field name, and every consumer that walks back the other way has
 * to invert exactly that derivation -- kept apart, the two drift, and the
 * symptom is a field that resolves from one feature but not another.
 */
object ProtoNaming {

  /** The outer class name protoc falls back to when it can derive nothing. */
  val DefaultOuterClassName = "OuterClass"

  /**
   * Suffix protoc appends to a derived outer class name that collides with a
   * top-level declaration in the same file.
   */
  private val OuterClassCollisionSuffix = "OuterClass"

  /**
   * Accessor prefixes generators put in front of a field name, longest first so
   * that stripping one is unambiguous (`addAll` before `add`).
   */
  val accessorPrefixes: Seq[String] =
    Seq(
      "addAll", "putAll", "contains", "remove", "clear", "get", "set", "has",
      "add", "put"
    )

  /**
   * Accessor suffixes that describe the field's cardinality or representation
   * rather than part of its name, longest first.
   */
  val accessorSuffixes: Seq[String] =
    Seq("OrDefault", "OrThrow", "Builder", "Bytes", "Count", "List", "Map")

  /**
   * Suffixes a generator appends to a proto declaration's name to form a
   * related type: the builder, the accessor interface, the gRPC wrapper and its
   * stubs, the file-derived outer class, the ScalaPB file object. Longest
   * first, so `OrBuilder` is not stripped as `Builder`.
   */
  val generatedTypeSuffixes: Seq[String] =
    Seq(
      "OrBuilder", "BlockingStub", "FutureStub", "ImplBase", "Builder",
      "OuterClass", "Stub", "Grpc", "Proto"
    )

  /** Suffixes of the gRPC stub classes generated for a service. */
  val grpcStubSuffixes: Seq[String] =
    Seq("ImplBase", "BlockingStub", "FutureStub", "Stub")

  /** `first_name` -> `FirstName`. */
  def snakeToUpperCamel(name: String): String =
    name.split('_').iterator.filter(_.nonEmpty).map(capitalize).mkString

  /** `first_name` -> `firstName`. */
  def snakeToLowerCamel(name: String): String =
    decapitalize(snakeToUpperCamel(name))

  /** `firstName` -> `first_name`. */
  def camelToSnake(name: String): String =
    if (name.isEmpty) name
    else {
      val builder = new StringBuilder
      builder.append(name.charAt(0).toLower)
      for (index <- 1 until name.length) {
        val char = name.charAt(index)
        if (char.isUpper) {
          builder.append('_')
          builder.append(char.toLower)
        } else {
          builder.append(char)
        }
      }
      builder.toString
    }

  def capitalize(name: String): String =
    if (name.isEmpty) name else name.head.toUpper + name.tail

  def decapitalize(name: String): String =
    if (name.isEmpty) name else name.head.toLower + name.tail

  /**
   * Whether the name is spelled the way proto enum values and constants are, in
   * which case no case conversion applies -- `HTTP_CODE` stays as it is instead
   * of becoming `h_t_t_p__c_o_d_e`.
   */
  def isAllCaps(name: String): Boolean =
    name.nonEmpty && name.forall(char =>
      char.isUpper || char == '_' || char.isDigit
    )

  /**
   * Case- and underscore-insensitive form, for comparing a proto name against
   * the generated spelling of it without asserting which convention was used.
   *
   * Deliberately lossy: it makes `example_type` equal to `exampleType` and to
   * `ExampleType`, so it must not be the only test when a type and a field of
   * the same name can both match. Compare kinds too.
   */
  def normalized(name: String): String =
    name.replace("_", "").toLowerCase(ju.Locale.ROOT)

  /**
   * The outer class protoc generates for a `.proto`, which carries every
   * declaration in the default layout and only static plumbing under
   * `java_multiple_files`.
   *
   * Nothing in the proto body names it: without `java_outer_classname` it comes
   * from the file name, so it cannot be recovered from the declarations alone.
   * When the derived name collides with a top-level declaration protoc appends
   * `OuterClass` rather than shadowing it.
   */
  def outerClassName(
      protoPath: String,
      explicitOuterClassName: Option[String],
      declaredTopLevelNames: Set[String]
  ): String =
    explicitOuterClassName.filter(_.nonEmpty).getOrElse {
      val derived = snakeToUpperCamel(protoBaseName(protoPath))
      if (derived.isEmpty) DefaultOuterClassName
      else if (declaredTopLevelNames.contains(derived))
        derived + OuterClassCollisionSuffix
      else derived
    }

  /** `a/b/foo_bar.proto` -> `foo_bar`. */
  def protoBaseName(protoPath: String): String =
    protoPath.split('/').lastOption.getOrElse("").stripSuffix(".proto")

  /**
   * Every Java accessor name a field of unknown type and cardinality could
   * produce. Generated as a set of candidates rather than the exact ones,
   * because the field's type is not known where this is called; consumers
   * validate a candidate against what the index actually holds.
   *
   * Inverse of [[protoFieldNameCandidates]].
   */
  def javaAccessorNames(protoFieldName: String): Seq[String] = {
    val camelCase = snakeToUpperCamel(protoFieldName)
    Seq(
      s"get$camelCase",
      s"has$camelCase",
      s"get${camelCase}Bytes",
      s"get${camelCase}List",
      s"get${camelCase}Count",
      s"get${camelCase}Map",
      s"contains$camelCase",
      s"get${camelCase}OrDefault",
      s"get${camelCase}OrThrow",
      s"set$camelCase",
      s"clear$camelCase",
      s"set${camelCase}Bytes",
      s"add$camelCase",
      s"addAll$camelCase",
      s"put$camelCase",
      s"putAll$camelCase",
      s"remove$camelCase"
    )
  }

  /**
   * The proto fields an accessor could be reading, most likely first: for
   * example `getFooBar` -> `foo_bar`, `setFooBarList` -> `foo_bar` then
   * `foo_bar_list`, `getHTTPCode` -> `HTTP_CODE`.
   *
   * Inverting the accessor name is lossy, which is why this yields candidates
   * rather than an answer. A field genuinely named `count` produces `getCount`,
   * indistinguishable from the repeated-field `getXCount` of a field named `x`;
   * a field named `foo_bar_list` produces `getFooBarList`, exactly like a
   * repeated `foo_bar`. Both readings are returned and the caller keeps the one
   * the proto actually declares.
   *
   * A name carrying no accessor prefix is a candidate field name in itself --
   * not every generator prefixes its accessors, and validating against declared
   * fields is what makes that safe.
   *
   * Inverse of [[javaAccessorNames]].
   */
  def protoFieldNameCandidates(accessorName: String): Seq[String] = {
    val withoutPrefix =
      accessorPrefixes
        .collectFirst {
          case prefix
              if accessorName.startsWith(prefix) &&
                accessorName.length > prefix.length =>
            accessorName.substring(prefix.length)
        }
        .getOrElse(accessorName)
    val withoutSuffix = accessorSuffixes.collectFirst {
      case suffix
          if withoutPrefix.endsWith(suffix) &&
            withoutPrefix.length > suffix.length =>
        withoutPrefix.stripSuffix(suffix)
    }
    val candidates = withoutSuffix.toSeq :+ withoutPrefix
    candidates.filter(_.nonEmpty).distinct.map { name =>
      if (isAllCaps(name)) name else camelToSnake(name)
    }
  }

  /**
   * The proto declaration names a generated type name could have come from:
   * itself, plus the name left once a generator suffix is removed, so
   * `UserOrBuilder` and `GreeterGrpc` reach `User` and `Greeter`.
   *
   * Candidates, in the order to try them -- the exact name first. A caller
   * keeps only those the proto actually declares.
   */
  def declarationNameCandidates(generatedTypeName: String): Seq[String] =
    generatedTypeName +: generatedTypeSuffixes.collect {
      case suffix
          if generatedTypeName.endsWith(suffix) &&
            generatedTypeName.length > suffix.length =>
        generatedTypeName.stripSuffix(suffix)
    }

  /** Whether the name is one of the gRPC stub classes generated per service. */
  def isGrpcStubName(typeName: String): Boolean =
    grpcStubSuffixes.exists(typeName.endsWith)

  /** The service a gRPC stub or wrapper class name belongs to. */
  def serviceNameOf(typeName: String): String =
    grpcStubSuffixes
      .collectFirst {
        case suffix
            if typeName.endsWith(suffix) && typeName.length > suffix.length =>
          typeName.stripSuffix(suffix)
      }
      .getOrElse(typeName)
      .stripSuffix("Grpc")
}
