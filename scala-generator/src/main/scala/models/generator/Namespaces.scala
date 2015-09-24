package scala.generator

import lib.generator.GeneratorUtil
import com.bryzek.apidoc.spec.v0.models.Service

object Namespaces {

  def quote(ns: String): String = {
    ns.split("\\.").map(ScalaUtil.quoteNameIfKeyword(_)).mkString(".")
  }

}

case class Namespaces(original: String) {

  val base = Namespaces.quote(original)

  val models: String = GeneratorUtil.fullyQualifiedName(base, GeneratorUtil.ObjectType.Model)
  val enums: String = GeneratorUtil.fullyQualifiedName(base, GeneratorUtil.ObjectType.Enum)
  val unions: String = GeneratorUtil.fullyQualifiedName(base, GeneratorUtil.ObjectType.Union)

  val errors: String = s"$base.errors"

  val last: String = base.split("\\.").last

  def importStatements(service: Service): Seq[String] = {
    Seq(
      if (service.enums.isEmpty) {
        None
      } else {
        Some(enums)
      },

      if (service.models.isEmpty) {
        None
      } else {
        Some(models)
      },

      if (service.unions.isEmpty) {
        None
      } else {
        Some(unions)
      }
).flatten.map { ns => s"import ${ns}._" }
  }

}
