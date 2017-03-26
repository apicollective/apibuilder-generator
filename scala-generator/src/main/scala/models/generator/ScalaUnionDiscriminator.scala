package scala.generator

import lib.Text._

case class ScalaUnionDiscriminator(
  union: ScalaUnion
) {
  val discriminator = union.discriminator.getOrElse {
    sys.error(s"ScalaUnionDiscriminator requires a discriminator - union[${union.name}] does not have one defined")
  }

  private[this] val className = s"${union.name}${underscoreToInitCap(discriminator)}"

  def build(): String = {
    import lib.Text._
    Seq(
      Seq(
        ScalaUtil.textToComment(s"Defines the valid ${discriminator} values for the type ${union.name}"),
        s"sealed trait $className"
      ).mkString("\n"),
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
    s"private[this] val byName = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
    s"def fromString(typ: String): _root_.scala.Option[$className] = byName.get(typ.toLowerCase)\n\n"
  }

}
