package scala.generator

import lib.generator.{GeneratorUtil, ObjectType}

object Namespaces {

  private[generator] val Parsers = "parsers"
  val Conversions = "conversions"

  def quote(ns: String): String = {
    ns.split("\\.").map(ScalaUtil.quoteNameIfKeyword).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base: String = Namespaces.quote(original)

  private val default: String = GeneratorUtil.fullyQualifiedImportName(base)
  val codeGenModels: String = default
  val codeGenEnums: String = default
  val codeGenUnions: String = default
  val codeGenInterfaces: String = default
  val originalInterfaces: String = GeneratorUtil.fullyQualifiedExternalName(base, ObjectType.Interface)

  val json: String = Seq(codeGenModels, "json").mkString(".")

  val anorm: String = Seq(base, "anorm").mkString(".")
  val anormParsers: String = Seq(anorm, Namespaces.Parsers).mkString(".")
  val anormConversions: String = Seq(anorm, Namespaces.Conversions).mkString(".")
  val errors: String = Seq(base, "errors").mkString(".")

  val mock: String = Seq(base, "mock").mkString(".")

  val last: String = base.split("\\.").last

  def getCodeGen(objectType: ObjectType): String = {
    objectType match {
      case ObjectType.Enum => codeGenEnums
      case ObjectType.Model => codeGenModels
      case ObjectType.Union => codeGenUnions
      case ObjectType.Interface => codeGenInterfaces
    }
  }

  def importStatements(): Seq[String] = {
    Seq(s"import $codeGenModels._")
  }

}
