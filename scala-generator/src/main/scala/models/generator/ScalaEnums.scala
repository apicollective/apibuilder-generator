package scala.generator

import lib.generator.Text

case class ScalaEnums(
  ssd: ScalaService,
  enum: ScalaEnum
) {

  private[this] val unions = ssd.unionsForEnum(enum)

  def build(): String = {
    import lib.generator.Text._
    Seq(
      enum.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
      s"sealed trait ${enum.name}" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse(""),
      s"object ${enum.name} {",
      buildValues().indent(2),
      s"}"
    ).mkString("\n\n")
  }

  /**
    * Returns the implicits for json serialization.
    */
  def buildJson(): String = {
    Seq(
      s"implicit val jsonReads${ssd.name}${enum.name} = __.read[String].map(${enum.name}.apply)",
      s"implicit val jsonWrites${ssd.name}${enum.name} = new Writes[${enum.name}] {",
      s"  def writes(x: ${enum.name}) = JsString(x.toString)",
      "}"
    ).mkString("\n")
  }

  private def buildValues(): String = {
    enum.values.map { value =>
      Seq(
        value.description.map { desc => ScalaUtil.textToComment(desc) },
        Some(s"""case object ${value.name} extends ${enum.name} { override def toString = "${value.originalName}" }""")
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
    s"val all = Seq(" + enum.values.map(_.name).mkString(", ") + ")\n\n" +
    s"private[this]\n" +
    s"val byName = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
    s"def apply(value: String): ${enum.name} = fromString(value).getOrElse(UNDEFINED(value))\n\n" +
    s"def fromString(value: String): _root_.scala.Option[${enum.name}] = byName.get(value.toLowerCase)\n\n"
  }

}
