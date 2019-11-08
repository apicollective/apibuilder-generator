package scala.models

import lib.generator.CodeGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Import
import scala.generator._

object ScalaCheckGenerator extends CodeGenerator {

  def header(form: InvocationForm) = ApidocComments(form.service.version, form.userAgent).toJavaString()

  def `import`(`import`: Import): String = s"""
    |import ${`import`.namespace}.models.scalacheck.${`import`.application.key.capitalize}Check._
  """.stripMargin.trim

  def model(namespace: String, model: ScalaModel): String = {
    def field(field: ScalaField): String = s"""
      |${field.name} <- Arbitrary.arbitrary[${field.datatype.name}]
    """.stripMargin.trim

    s"""
      |implicit def arbitrary${model.name}: Arbitrary[${namespace}.${model.name}] = Arbitrary(gen${model.name})
      |def gen${model.name}: Gen[${namespace}.${model.name}] = for {
      |  ${model.fields.map(field).mkString("\n")}
      |} yield ${namespace}.${model.name}(${model.fields.map(_.name).mkString(", ")})
    """.stripMargin.trim
  }

  def enum(namespace: String, enum: ScalaEnum): String = {
    val gens = enum.values.map(value => s"${namespace}.${enum.name}.${value.name}").toList
    val gen = gens match {
      case Nil => "???"
      case one :: Nil => one
      case more => s"Gen.oneOf(${more.mkString(", ")})"
    }

    s"""
      |implicit def arbitrary${enum.name}: Arbitrary[${namespace}.${enum.name}] = Arbitrary(gen${enum.name})
      |def gen${enum.name}: Gen[${namespace}.${enum.name}] = ${gen}
    """.stripMargin.trim
  }

  def union(namespace: String, union: ScalaUnion): String = {
    val gens = union.types.map(`type` => s"gen${`type`.name}").toList
    val gen = gens match {
      case Nil => "???"
      case one :: Nil => one
      case more => s"Gen.oneOf(${more.mkString(", ")})"
    }

    s"""
      |implicit def arbitrary${union.name}: Arbitrary[${namespace}.${union.name}] = Arbitrary(gen${union.name})
      |def gen${union.name}: Gen[${namespace}.${union.name}] = ${gen}
    """.stripMargin.trim
  }

  def jodaDateTime(): String = s"""
    |implicit def arbitraryJodaDateTime: Arbitrary[_root_.org.joda.time.DateTime] = Arbitrary(genJodaDateTime)
    |def genJodaDateTime: Gen[_root_.org.joda.time.DateTime] = Gen.posNum[Long].map(instant => new _root_.org.joda.time.DateTime(instant))
  """.stripMargin.trim

  def contents(form: InvocationForm): String = {
    val ssd = ScalaService(form.service)
    s"""
    |${header(form)}
    |package ${ssd.namespaces.models}.scalacheck
    |
    |${form.service.imports.map(`import`).mkString("\n")}
    |import org.scalacheck.{Arbitrary, Gen}
    |
    |object ${ssd.name}Check extends ${ssd.name}Check
    |trait ${ssd.name}Check {
    |
    |  ${jodaDateTime}
    |
    |  ${ssd.models.map(model(ssd.namespaces.models, _)).mkString("\n\n")}
    |
    |  ${ssd.enums.map(enum(ssd.namespaces.models,_)).mkString("\n\n")}
    |
    |  ${ssd.unions.map(union(ssd.namespaces.models, _)).mkString("\n\n")}
    |
    |}
  """.stripMargin.trim
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    Right(Seq(
        File("", None, contents(form), None)
    ))
  }
}
