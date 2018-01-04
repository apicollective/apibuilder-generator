package scala.generator

case class ScalaEnums(
  ssd: ScalaService,
  enum: ScalaEnum
) {

  private[this] val unions = ssd.unionsForEnum(enum)

  def build(): String = {
    import lib.Text._
    Seq(
      enum.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
      s"sealed trait ${enum.name}" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse(" extends _root_.scala.Product with _root_.scala.Serializable"),
      s"${ScalaUtil.deprecationString(enum.deprecation)}object ${enum.name} {",
      buildValues().indent(2),
      s"}"
    ).mkString("\n\n")
  }

  private[this] def buildValues(): String = {
    enum.values.map { value =>
      Seq(
        value.description.map { desc => ScalaUtil.textToComment(desc) },
        Some(s"""${ScalaUtil.deprecationString(value.deprecation)}case object ${value.name} extends ${enum.name} { override def toString = "${value.serializedValue}" }""")
      ).flatten.mkString("\n")
    }.mkString("\n") + "\n" +
    s"""
/**
 * UNDEFINED captures values that are sent either in error or
 * that were added by the server after this library was
 * generated. We want to make it easy and obvious for users of
 * this library to handle this case gracefully.
 *
 * We use all CAPS for the variable name to avoid collisions
 * with the camel cased values above.
 */
case class UNDEFINED(override val toString: String) extends ${enum.name}

/**
 * all returns a list of all the valid, known values. We use
 * lower case to avoid collisions with the camel cased values
 * above.
 */
""" +
    s"val all: scala.List[${enum.name}] = scala.List(" + enum.values.map(_.name).mkString(", ") + ")\n\n" +
    s"private[this]\n" +
    s"val byName: Map[String, ${enum.name}] = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
      s"def apply(value: String)(implicit onUndefined: String => Unit = _ => {}): ${enum.name} = fromString(value).getOrElse {\n" +
      s"\tonUndefined(value)\n" +
      s"\tUNDEFINED(value)\n" +
      s"}\n\n" +
    s"def fromString(value: String): _root_.scala.Option[${enum.name}] = byName.get(value.toLowerCase)\n\n"
  }

}
