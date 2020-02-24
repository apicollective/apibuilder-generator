package scala.generator

import lib.generator.GeneratorUtil

object Namespaces {

  val Parsers = "parsers"
  val Conversions = "conversions"

  def quote(ns: String): String = {
    ns.split("\\.").map(ScalaUtil.quoteNameIfKeyword).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base: String = Namespaces.quote(original)

  val models: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Model)
  val enums: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Enum)
  val unions: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Union)

  val json: String = Seq(models, "json").mkString(".")

  val anorm: String = Seq(base, "anorm").mkString(".")
  val anormParsers: String = Seq(anorm, Namespaces.Parsers).mkString(".")
  val anormConversions: String = Seq(anorm, Namespaces.Conversions).mkString(".")
  val errors: String = Seq(base, "errors").mkString(".")

  val mock: String = Seq(base, "mock").mkString(".")
  val interfaces: String = Seq(base, "interfaces").mkString(".")

  val last: String = base.split("\\.").last

  def get(objectType: GeneratorUtil.ObjectType): String = {
    objectType match {
      case GeneratorUtil.ObjectType.Enum => enums
      case GeneratorUtil.ObjectType.Model => models
      case GeneratorUtil.ObjectType.Union => unions
    }
  }

  def importStatements(): Seq[String] = {
    Seq(s"import $models._")
  }

}
