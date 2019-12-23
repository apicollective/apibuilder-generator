package scala.models

import lib.generator.CodeGenerator
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Import
import scala.generator._

object ScalaCheckGenerator extends CodeGenerator {

  def fileName(str: String): String = s"${objectName(str)}.scala"
  def packageName(str: String): String = s"${str}.models.scalacheck"
  def objectName(str: String): String = s"${str}ScalaCheck"
  def traitName(str: String): String = objectName(str)

  def header(form: InvocationForm) = ApidocComments(form.service.version, form.userAgent).toJavaString()

  def extendsWith(imports: Seq[Import]): String = {
    val traits = imports.map(i => s"${packageName(i.namespace)}.${traitName(i.application.key.capitalize)}").toList
    traits match {
      case Nil => ""
      case one :: Nil => s"extends ${one}"
      case more => more.mkString("extends ", " with ", "")
    }
  }

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

  def jodaDateTime(imports: Seq[Import]): String = {
    val alreadyDefined = if(imports.isEmpty) { "" } else { "override" }
    s"""
      |implicit ${alreadyDefined} def arbitraryJodaDateTime: Arbitrary[_root_.org.joda.time.DateTime] = Arbitrary(genJodaDateTime)
      |${alreadyDefined} def genJodaDateTime: Gen[_root_.org.joda.time.DateTime] = Gen.posNum[Long].map(instant => new _root_.org.joda.time.DateTime(instant))
    """.stripMargin.trim
  }

  def contents(form: InvocationForm, ssd: ScalaService): String =
    s"""
      |${header(form)}
      |package ${packageName(ssd.namespaces.base)}
      |
      |import org.scalacheck.{Arbitrary, Gen}
      |
      |object ${objectName(ssd.name)} extends ${traitName(ssd.name)}
      |trait ${traitName(ssd.name)} ${extendsWith(form.service.imports)} {
      |
      |  ${jodaDateTime(form.service.imports)}
      |
      |  ${ssd.models.map(model(ssd.namespaces.models, _)).mkString("\n\n")}
      |
      |  ${ssd.enums.map(enum(ssd.namespaces.models,_)).mkString("\n\n")}
      |
      |  ${ssd.unions.map(union(ssd.namespaces.models, _)).mkString("\n\n")}
      |
      |}
    """.stripMargin.trim

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form.service)
    Right(Seq(
        File(fileName(ssd.name), None, contents(form, ssd), None)
    ))
  }
}
