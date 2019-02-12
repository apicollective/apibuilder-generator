package scala.models.play.files

import io.apibuilder.generator.v0.models.InvocationForm
import scala.generator._

object ModelsGens {

  val Arbitrary = "_root_.org.scalacheck.Arbitrary"
  val Gen = "_root_.org.scalacheck.Gen"

  val JodaDateTime = "_root_.org.joda.time.DateTime"
  val JodaLocalDate = "_root_.org.joda.time.LocalDate"
  val JsObject = "_root_.play.api.libs.json.JsObject"
  val JsValue = "_root_.play.api.libs.json.JsValue"
  val JsArray = "_root_.play.api.libs.json.JsArray"
  val JsBoolean = "_root_.play.api.libs.json.JsBoolean"
  val JsNull = "_root_.play.api.libs.json.JsNull"
  val JsNumber = "_root_.play.api.libs.json.JsNumber"
  val JsString = "_root_.play.api.libs.json.JsString"

  def jodaDateTimeGenAndArbitrary(ns: Namespaces): String = s"""
    private[${ns.last}] ${arbitrary(ns, "JodaDateTime", JodaDateTime)}
    private[${ns.last}] lazy val genJodaDateTime: ${Gen}[${JodaDateTime}] = ${Gen}.lzy {
      ${Gen}.posNum[Long].map(instant => new ${JodaDateTime}(instant))
    }
  """

  def jodaLocalDateGenAndArbitrary(ns: Namespaces): String = s"""
    private[${ns.last}] ${arbitrary(ns, "JodaLocalDate", JodaLocalDate)}
    private[${ns.last}] lazy val genJodaLocalDate: ${Gen}[${JodaLocalDate}] = ${Gen}.lzy {
      ${Gen}.posNum[Long].map(instant => new ${JodaLocalDate}(instant))
    }
  """

  def playJsObjectGenAndArbitrary(ns: Namespaces): String = s"""
    private[${ns.last}] ${arbitrary(ns, "JsObject", JsObject)}
    private[${ns.last}] lazy val genJsObject: ${Gen}[${JsObject}] = ${Gen}.lzy {
      for {
        underlying <- ${Arbitrary}.arbitrary[Map[String, ${JsValue}]]
      } yield ${JsObject}(underlying)
    }
  """

  def playJsValueGenAndArbitrary(ns: Namespaces): String = s"""
    private[${ns.last}] ${arbitrary(ns, "JsValue", JsValue)}
    private[${ns.last}] lazy val genJsValue: ${Gen}[${JsValue}] = ${Gen}.lzy {
      ${Gen}.oneOf(
        ${Arbitrary}.arbitrary[IndexedSeq[${JsValue}]].map(${JsArray}),
        ${Arbitrary}.arbitrary[Boolean].map(${JsBoolean}),
        ${Gen}.const(${JsNull}),
        ${Arbitrary}.arbitrary[BigDecimal].map(${JsNumber}),
        // ${Arbitrary}.arbitrary[${JsObject}],
        ${Arbitrary}.arbitrary[String].map(${JsString})
      )
    }
  """

  def imports(model: ScalaModel): List[String] = model.fields.flatMap { field => imports(model.ssd.namespaces, field.datatype) }
  def imports(currentNamespace: Namespaces, tpe: ScalaDatatype): Option[String] = {
    val ns = tpe match {
      case ScalaPrimitive.Enum(ns, _) if currentNamespace != ns => Some(ns)
      case ScalaPrimitive.Model(ns, _) if currentNamespace != ns => Some(ns)
      case ScalaPrimitive.Union(ns, _) if currentNamespace != ns => Some(ns)
      case _ => None
    }

    ns.map { ns => s"import ${ns.models}.gens._" }
  }

	def arbitrary(enum: ScalaEnum): String = arbitrary(enum.ssd.namespaces, enum.name, enum.qualifiedName)
	def arbitrary(model: ScalaModel): String = arbitrary(model.ssd.namespaces, model.name, model.qualifiedName)
	def arbitrary(union: ScalaUnion): String = arbitrary(union.ssd.namespaces, union.name, union.qualifiedName)
  def arbitrary(ns: Namespaces, name: String, tpe: String): String = {
    val collisionFreeName = s"""${ns.models.split('.').map(_.capitalize).mkString}${name}"""
    s"""implicit lazy val arbitrary${collisionFreeName}: ${Arbitrary}[$tpe] = ${Arbitrary}(gen${name})"""
  }

  def genOneOf(name: String, tpe: String, oneOf: List[String]): String = oneOf match {
    case Nil => ""
    case one :: Nil => s"lazy val gen${name}: ${Gen}[${tpe}] = ${one}"
    case list => s"""
      lazy val gen${name}: ${Gen}[${tpe}] = ${Gen}.lzy {
        ${Gen}.oneOf(${list.mkString(", ")})
      }
    """
  }

  def genFor(name: String, tpe: String, properties: List[(String, String)]) = properties match {
    case Nil => ""
    case properties =>
      val arguments = properties.unzip._1
      val enumerators = properties.map { case (name, tpe) => s"""${name} <- ${Arbitrary}.arbitrary[${tpe}]""" }

      s"""
        lazy val gen${name}: ${Gen}[${tpe}] = ${Gen}.lzy {
          for {
            ${enumerators.mkString("\n")}
          } yield ${tpe}(${arguments.mkString(",")})
        }
      """
  }

  def gen(enum: ScalaEnum): String = {
    val oneOf = enum.values.map(v => s"${Gen}.const(${enum.qualifiedName}.${v.name})").toList
    genOneOf(enum.name, enum.qualifiedName, oneOf)
  }

  def gen(model: ScalaModel): String = {
    val args = model.fields.map { field => (field.name, field.datatype.name) }
    genFor(model.name, model.qualifiedName, args)
  }

	def gen(union: ScalaUnion): String = {
    val oneOf = union.types.map {t =>
      val tpe = ScalaUnionType.typeName(union, t)
      s"${Arbitrary}.arbitrary[${tpe}]"
    }.toList

    genOneOf(union.name, union.qualifiedName, oneOf)
  }

	def contents(form: InvocationForm): String = {
    val scalaService = ScalaService(form.service)
    val wrappers = PrimitiveWrapper(scalaService).wrappers

    val imports = scalaService.models.flatMap(this.imports)
      .distinct

    val arbitraries =
      scalaService.enums.map(arbitrary) ++
      scalaService.models.map(arbitrary) ++
      scalaService.unions.map(arbitrary) ++
      wrappers.map(w => arbitrary(w.model))

		val gens =
      scalaService.enums.map(gen) ++
      scalaService.models.map(gen) ++
      scalaService.unions.map(gen) ++
      wrappers.map(w => gen(w.model))

    s"""
      package ${scalaService.namespaces.models}

      package object gens {

        ${jodaDateTimeGenAndArbitrary(scalaService.namespaces)}
        ${jodaLocalDateGenAndArbitrary(scalaService.namespaces)}

        ${playJsObjectGenAndArbitrary(scalaService.namespaces)}
        ${playJsValueGenAndArbitrary(scalaService.namespaces)}

        ${imports.mkString("\n")}

        ${arbitraries.mkString("\n")}

				${gens.mkString("\n")}

      }
    """
  }


}
