package models.generator.kotlin

import lib.generator.GeneratorUtil
import io.apibuilder.spec.v0.models.Service

object Namespaces {

  def quote(ns: String): String = {
    ns.split("\\.").map(KotlinUtil.quoteNameIfReservedWord).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base: String = Namespaces.quote(original)

  val models: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Model)
  val enums: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Enum)
  val unions: String = GeneratorUtil.fullyQualifiedInternalName(base, GeneratorUtil.ObjectType.Union)

  val json: String = Seq(models, "json").mkString(".")

  val mock: String = Seq(base, "mock").mkString(".")

  val last: String = base.split("\\.").last

  def get(objectType: GeneratorUtil.ObjectType): String = {
    objectType match {
      case GeneratorUtil.ObjectType.Enum => enums
      case GeneratorUtil.ObjectType.Model => models
      case GeneratorUtil.ObjectType.Union => unions
    }
  }

  def importStatements(service: Service): Seq[String] = {
    Seq(s"import $models._")
  }

}
