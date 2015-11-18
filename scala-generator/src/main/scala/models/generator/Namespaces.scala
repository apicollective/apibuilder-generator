package scala.generator

import lib.generator.GeneratorUtil
import com.bryzek.apidoc.spec.v0.models.Service

object Namespaces {

  val Parsers = "parsers"
  val Conversions = "conversions"

  def quote(ns: String): String = {
    ns.split("\\.").map(ScalaUtil.quoteNameIfKeyword(_)).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base = Namespaces.quote(original)

  val models: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Model)
  val enums: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Enum)
  val unions: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Union)

  val anorm: String = Seq(base, "anorm").mkString(".")
  val anormParsers: String = Seq(anorm, Namespaces.Parsers).mkString(".")
  val anormConversions: String = Seq(anorm, Namespaces.Conversions).mkString(".")
  val errors: String = Seq(base, "errors").mkString(".")

  val last: String = base.split("\\.").last

  def get(objectType: GeneratorUtil.ObjectType): String = {
    objectType match {
      case GeneratorUtil.ObjectType.Enum => enums
      case GeneratorUtil.ObjectType.Model => models
      case GeneratorUtil.ObjectType.Union => unions
    }
  }

  def importStatements(service: Service): Seq[String] = {
    Seq(s"import ${models}._")
  }

}
