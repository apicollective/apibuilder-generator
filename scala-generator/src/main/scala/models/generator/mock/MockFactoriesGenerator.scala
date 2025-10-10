package scala.generator.mock

import generator.ServiceFileNames
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import lib.Text
import lib.Text._
import lib.generator.CodeGenerator

import scala.generator._
import scala.models.{ApiBuilderComments, Attributes}

object MockFactoriesGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val ssd = ScalaService(form, Attributes.PlayDefaultConfig)
    new MockFactoriesGenerator(ssd, form.userAgent).invoke()
  }

  private[mock] def factoriesCode(ssd: ScalaService): String = Seq(
    "object Factories {",
    Seq(
      """def randomString(length: Int = 24): String = {""",
      """  _root_.scala.util.Random.alphanumeric.take(length).mkString""",
      """}"""
    ).mkString("\n").indentString(2),
    Seq(
      ssd.enums.map { makeEnum },
      ssd.models.map { makeModel },
      ssd.unions.map { makeUnion }
    ).flatten.mkString("\n\n").indentString(2),
    "}"
  ).mkString("\n\n")

  private[mock] def makeEnum(`enum`: ScalaEnum): String = {
    val name = enum.values.headOption match {
      case None => {
        """UNDEFINED("other")"""
      }
      case Some(value) => {
        value.name
      }
    }
    s"def make${enum.name}(): ${enum.qualifiedName} = ${enum.qualifiedName}.$name"
  }

  private[mock] def makeModel(model: ScalaModel): String = {
    Seq(
      s"def make${model.name}(): ${model.qualifiedName} = ${model.qualifiedName}(",
      model.fields.map { field =>
        s"${field.name} = ${mockValue(field.datatype, Some(field.limitation))}"
      }.mkString(",\n").indentString(2),
      ")"
    ).mkString("\n")
  }

  private[mock] def makeUnion(union: ScalaUnion): String = {
    val impl = union.types.headOption.map(_.datatype) match {
      case Some(datatype: ScalaPrimitive) => {
        val value = mockValue(datatype)
        if (needsWrapper(datatype)) {
          val className = Text.pascalCase(union.name) + Text.pascalCase(datatype.shortName)
          union.ssd.namespaces.codeGenUnions + "." + className + s"($value)"
        } else {
          value
        }
      }
      case _ => "???" // no types defined
    }
    s"def make${union.name}(): ${union.qualifiedName} = $impl"
  }

  private def needsWrapper(datatype: ScalaDatatype): Boolean = {
    datatype match {
      case _: ScalaPrimitive.Enum | _: ScalaPrimitive.Model | _: ScalaPrimitive.GeneratedModel | _: ScalaPrimitive.Union => {
        false
      }
      case _ => true
    }
  }

  private[mock] def mockValue(
    datatype: ScalaDatatype,
    limitation: Option[ScalaField.Limitation] = None,
    unitType: String = "// unit type",
  ): String = {
    datatype match {
      case ScalaPrimitive.Boolean => "true"
      case ScalaPrimitive.Double => "1.0"
      case ScalaPrimitive.Integer => "1"
      case ScalaPrimitive.Long => "1L"
      case dt: ScalaPrimitive.DateIso8601 => s"${dt.fullName}.now"
      case dt: ScalaPrimitive.DateTimeIso8601 => s"${dt.fullName}.now"
      case ScalaPrimitive.Decimal => """BigDecimal("1")"""
      case ScalaPrimitive.ObjectAsPlay => "_root_.play.api.libs.json.Json.obj()"
      case ScalaPrimitive.ObjectAsCirce => "Map()"
      case ScalaPrimitive.JsonValueAsPlay => "_root_.play.api.libs.json.Json.obj().asInstanceOf[_root_.play.api.libs.json.JsValue]"
      case dt@ScalaPrimitive.JsonValueAsCirce => s"${dt.asInstanceOf[ScalaPrimitive].fullName}.obj()"
      case ScalaPrimitive.String => {
        limitation match {
          case None => "Factories.randomString()"
          case Some(limitationVal) => s"Factories.randomString(${calculateStringLength(limitationVal)})"
        }
      }
      case ScalaPrimitive.Unit => unitType
      case dt@ScalaPrimitive.Uuid => s"${dt.asInstanceOf[ScalaPrimitive].fullName}.randomUUID"
      case ScalaDatatype.List(_) => "Nil"
      case ScalaDatatype.Map(_) => "Map()"
      case ScalaDatatype.Option(_) => "None"
      case ScalaPrimitive.Enum(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Model(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.Union(ns, name) => s"${ns.mock}.Factories.make$name()"
      case ScalaPrimitive.GeneratedModel(_) => sys.error("Generated models should not be mocked")
    }
  }

  private[mock] def calculateStringLength(limitation: ScalaField.Limitation): Int = {
    val defaultCandidate = 24L

    // TODO handle Int vs Long (really String of length Int.MaxValue + 1??!)
    //   consider some artificial upper bound of e.g. 10,000; 100,000; 1,000,000 -- to be discussed

    val withLimitationApplied = limitation match {
      case ScalaField.Limitation(None, None) => defaultCandidate
      case ScalaField.Limitation(Some(minimum), None) => Math.max(defaultCandidate, minimum)
      case ScalaField.Limitation(None, Some(maximum)) => Math.min(defaultCandidate, maximum)
      case ScalaField.Limitation(Some(minimum), Some(maximum)) =>
        if (minimum < maximum) (minimum + maximum) / 2
        else if (minimum == maximum) minimum
        else 0
    }

    val withZeroAsLowerBound = Math.max(0L, withLimitationApplied)

    if (!withZeroAsLowerBound.isValidInt) Int.MaxValue // TODO really?!
    else withZeroAsLowerBound.toInt
  }
}

private[mock] class MockFactoriesGenerator(
  ssd: ScalaService,
  userAgent: Option[String],
) {
  import MockFactoriesGenerator._

  def invoke(): Either[Seq[String], Seq[File]] = {
    val header = ApiBuilderComments(ssd.service.version, userAgent).toJavaString + "\n"
    val code = generateCode()

    Right(
      Seq(
        ServiceFileNames.toFile(
          ssd.service.namespace,
          ssd.service.organization.key,
          ssd.service.application.key,
          ssd.service.version,
          "MockFactories",
          header ++ code,
          Some("Scala")
        )
      )
    )
  }

  def generateCode(): String = {
    Seq(
      s"package ${ssd.namespaces.mock} {",
      factoriesCode(ssd).indentString(2),
      "}"
    ).mkString("\n\n")
  }
}
