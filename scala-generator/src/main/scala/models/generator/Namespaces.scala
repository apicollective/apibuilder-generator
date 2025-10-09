package scala.generator

import lib.generator.GeneratorUtil

object Namespaces {

  private[generator] val Parsers = "parsers"
  val Conversions = "conversions"

  def quote(ns: String): String = {
    ns.split("\\.").map(ScalaUtil.quoteNameIfKeyword).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base: String = Namespaces.quote(original)

  val models: String = GeneratorUtil.fullyQualifiedImportName(base)
  val enums: String = GeneratorUtil.fullyQualifiedImportName(base)
  val unions: String = GeneratorUtil.fullyQualifiedImportName(base)
  val interfaces: String = GeneratorUtil.fullyQualifiedImportName(base)

  val json: String = Seq(models, "json").mkString(".")

  val anorm: String = Seq(base, "anorm").mkString(".")
  val anormParsers: String = Seq(anorm, Namespaces.Parsers).mkString(".")
  val anormConversions: String = Seq(anorm, Namespaces.Conversions).mkString(".")
  val errors: String = Seq(base, "errors").mkString(".")

  val mock: String = Seq(base, "mock").mkString(".")

  val last: String = base.split("\\.").last

  def get(objectType: GeneratorUtil.ObjectType): String = {
    objectType match {
      case GeneratorUtil.ObjectType.Enum => enums
      case GeneratorUtil.ObjectType.Model => models
      case GeneratorUtil.ObjectType.Union => unions
      case GeneratorUtil.ObjectType.Interface => interfaces
    }
  }

  def importStatements(): Seq[String] = {
    Seq(s"import $models._")
  }

}
