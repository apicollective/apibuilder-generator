package scala.generator

import lib.Text._

case class ScalaUnionDiscriminator(
  union: ScalaUnion
) {
  val discriminator: String = union.discriminator.getOrElse {
    sys.error(s"ScalaUnionDiscriminator requires a discriminator - union[${union.name}] does not have one defined")
  }

  val className = s"${union.name}${underscoreToInitCap(discriminator)}"

  def build(): String = {
    Seq(
      Seq(
        ScalaUtil.textToComment(s"Defines the valid $discriminator values for the type ${union.name}"),
        s"sealed trait $className extends _root_.scala.Product with _root_.scala.Serializable"
      ).mkString("\n"),
      Seq(
        Seq(
          ScalaUtil.deprecationString(union.deprecation),
          s"object $className {",
          buildTypes().indent(2),
          s"}"
        ).mkString("\n")
      ).mkString("\n")
    ).mkString("\n\n")
  }

  private[this] def buildTypes(): String = {
    Seq(
      union.types.map { typ =>
        Seq(
          typ.description.map { desc => ScalaUtil.textToComment(desc) },
          ScalaUtil.deprecationString(typ.deprecation) match {
            case "" => None
            case v => Some(v)
          },
          Some(s"""case object ${typ.name} extends $className { override def toString = "${typ.originalName}" }""")
        ).flatten.mkString("\n")
      }.mkString("\n"),
      s"case class UNDEFINED(override val toString: String) extends $className",
      s"val all: scala.List[$className] = scala.List(" + union.types.map(_.name).mkString(", ") + ")",
      s"private[this] val byName: Map[String, $className] = all.map(x => x.toString.toLowerCase -> x).toMap",
      s"def apply(value: String): $className = fromString(value).getOrElse(UNDEFINED(value))",
      s"def fromString(value: String): _root_.scala.Option[$className] = byName.get(value.toLowerCase)"
    ).mkString("\n\n")
  }

}
