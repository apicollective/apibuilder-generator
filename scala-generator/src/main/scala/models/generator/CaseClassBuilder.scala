package scala.generator

import io.apibuilder.spec.v0.models.Deprecation

private[generator] case class CaseClassBuilder(
  name: Option[String] = None,
  extendsClasses: Seq[String] = Nil,
  deprecation: Option[Deprecation] = None,
  scaladoc: Option[String] = None,
  argList: Option[String] = None,
  bodyParts: Seq[String] = Nil,
) {
  private[this] val MaxLineLength = 120

  def withName(name: String): CaseClassBuilder = {
    this.copy(name = Some(name))
  }

  def withDeprecation(deprecation: Option[Deprecation]): CaseClassBuilder = {
    this.copy(deprecation = deprecation)
  }

  def withScaladoc(scaladoc: Option[String]): CaseClassBuilder = {
    this.copy(scaladoc = scaladoc)
  }

  def withArgList(argList: Option[String]): CaseClassBuilder = {
    this.copy(argList = argList)
  }

  def withExtendsClasses(classNames: Seq[String]): CaseClassBuilder = {
    this.copy(
      extendsClasses = extendsClasses ++ classNames
    )
  }

  def withBodyParts(parts: Seq[String]): CaseClassBuilder = {
    this.copy(
      bodyParts = bodyParts ++ parts
    )
  }

  private[this] def buildClassType: String = {
    if (argList.isEmpty) {
      "case object"
    } else {
      "final case class"
    }
  }

  private[this] def buildArgs: String = {
    argList match {
      case None => ""
      case Some(a) => s"($a)"
    }
  }

  private[this] def buildDeclaration: String = {
    val n = name.getOrElse(
      sys.error("Name must be provided")
    )
    val base = s"$buildClassType $n$buildArgs"
    extendsClasses.distinct.sorted.toList match {
      case Nil => base
      case one :: Nil => s"$base extends $one"
      case one :: rest => s"$base extends $one with ${rest.mkString(" with ")}"
    }
  }

  private[this] def buildDeprecation: Option[String] = {
    Some(ScalaUtil.deprecationString(deprecation).trim).filter(_.nonEmpty)
  }

  def build: String = {
    val base = Seq(
      scaladoc,
      buildDeprecation,
      Some(buildDeclaration),
    ).flatten.mkString("\n").strip

    bodyParts.filter(_.trim.nonEmpty).toList match {
      case Nil => base
      case one :: Nil if base.length + one.length + 5 <= MaxLineLength => s"$base { ${one.trim} }"
      case parts => {
        Seq(
          s"$base {",
          parts.mkString("\n").trim.indent(2).stripTrailing(),
          "}",
        ).mkString("\n")
      }
    }
  }
}