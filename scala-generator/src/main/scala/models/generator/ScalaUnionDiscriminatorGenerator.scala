package scala.generator

import lib.Text._

case class ScalaUnionDiscriminatorGenerator(
  union: ScalaUnion
) {
  private val discriminator: ScalaUnionDiscriminator = union.discriminatorField.getOrElse {
    sys.error(s"ScalaUnionDiscriminator requires a discriminator - union[${union.name}] does not have one defined")
  }

  private val className = discriminator.field.field.`type`

  def build(): String = {
    Seq(
      Seq(
        ScalaUtil.textToComment(s"Defines the valid ${discriminator.discriminator} values for the type ${union.name}"),
        s"sealed trait $className extends _root_.scala.Product with _root_.scala.Serializable"
      ).mkString("\n"),
      Seq(
        ScalaUtil.deprecationString(union.deprecation).trim match {
          case "" => None
          case v => Some(v)
        },
        Some(
          Seq(
            s"object $className {",
            buildTypes().indentString(2),
            union.defaultType.map { t =>
              s"  val default: $className = ${t.name}"
            }.getOrElse(""),
            s"}"
          ).filterNot(_.isEmpty).mkString("\n\n")
        )
      ).flatten.mkString("\n")
    ).mkString("\n\n")
  }

  private def buildTypes(): String = {
    Seq(
      union.types.map { typ =>
        Seq(
          typ.description.map { desc => ScalaUtil.textToComment(desc) },
          ScalaUtil.deprecationString(typ.deprecation) match {
            case "" => None
            case v => Some(v)
          },
          Some(s"""case object ${typ.name} extends $className { override def toString = "${typ.discriminatorName}" }""")
        ).flatten.mkString("\n")
      }.mkString("\n"),
      s"final case class UNDEFINED(override val toString: String) extends $className",
      s"val all: scala.List[$className] = scala.List(" + union.types.map(_.name).mkString(", ") + ")",
      s"private val byName: Map[String, $className] = all.map(x => x.toString.toLowerCase -> x).toMap",
      s"def apply(value: String): $className = fromString(value).getOrElse(UNDEFINED(value))",
      s"def fromString(value: String): _root_.scala.Option[$className] = byName.get(value.toLowerCase)"
    ).mkString("\n\n")
  }

}
