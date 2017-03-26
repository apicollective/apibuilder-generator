package scala.generator

import scala.models.Play2JsonCommon
import lib.Text._

case class ScalaUnionDiscriminator(
  union: ScalaUnion
) {
  val discriminator = union.discriminator.getOrElse {
    sys.error(s"ScalaUnionDiscriminator requires a discriminator - union[${union.name}] does not have one defined")
  }

  private[this] val className = s"${union.name}Discriminator"

  def build(): String = {
    import lib.Text._
    Seq(
      ScalaUtil.textToComment(s"Defines the valid values for the ${union.discriminator} for the union type ${union.name}"),
      s"sealed trait $className",
      s"${ScalaUtil.deprecationString(union.deprecation)}object $className {",
      buildTypes().indent(2),
      s"}"
    ).mkString("\n\n")
  }

  private[this] def buildTypes(): String = {
    union.types.map { typ =>
      Seq(
        typ.description.map { desc => ScalaUtil.textToComment(desc) },
        Some(s"""${ScalaUtil.deprecationString(typ.deprecation)}case object ${typ.name} extends $className { override def toString = "${typ.originalName}" }""")
      ).flatten.mkString("\n")
    }.mkString("\n") + "\n\n" +
    s"val all = Seq(" + union.types.map(_.name).mkString(", ") + ")\n\n" +
    s"private[this]\n" +
    s"val byName = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
    s"def fromString(typ: String): _root_.scala.Option[${union.name}] = byName.get(typ.toLowerCase)\n\n"
  }

}
