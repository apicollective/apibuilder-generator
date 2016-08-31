package scala.models

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.Text._
import lib.generator.CodeGenerator
import scala.generator.{ScalaEnums, ScalaCaseClasses, ScalaService}
import generator.ServiceFileNames

object Play2Models extends Play2Models

trait Play2Models extends CodeGenerator {

  override def invoke(
    form: InvocationForm
  ): Either[Seq[String], Seq[File]] = {
    ScalaCaseClasses.modelsWithTooManyFieldsErrors(form.service) match {
      case Nil => Right(generateCode(form = form, addBindables = true, addHeader = true))
      case errors => Left(errors)
    }
  }

  def generateCode(
    form: InvocationForm,
    addBindables: Boolean,
    addHeader: Boolean
  ): Seq[File] = {
    val ssd = ScalaService(form.service)

    val caseClasses = ScalaCaseClasses.generateCode(form, addHeader = false).map(_.contents).mkString("\n\n")
    val prefix = underscoreAndDashToInitCap(ssd.name)
    val enumJson: String = ssd.enums.map { ScalaEnums(ssd, _).buildJson() }.mkString("\n\n")
    val play2Json = Play2Json(ssd).generate()

    val header = addHeader match {
      case false => ""
      case true => ApidocComments(form.service.version, form.userAgent).toJavaString() + "\n"
    }

    val bindables = addBindables match {
      case false => ""
      case true => {
        "\n" +
        Seq(
          s"package ${ssd.namespaces.base} {",
          Play2Bindables(ssd).build().indent(2),
          "}"
        ).mkString("\n\n")
      }
    }

    val source = s"""$header$caseClasses

package ${ssd.namespaces.models} {

  package object json {
    import play.api.libs.functional.syntax._
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.json.Reads.DefaultJodaDateReads
${JsonImports(form.service).mkString("\n").indent(4)}

    private[${ssd.namespaces.last}] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

    private[${ssd.namespaces.last}] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
      def writes(x: java.util.UUID) = JsString(x.toString)
    }

    private[${ssd.namespaces.last}] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
      def writes(x: org.joda.time.DateTime) = {
        import org.joda.time.format.ISODateTimeFormat.dateTime
        val str = dateTime.print(x)
        JsString(str)
      }
    }

${Seq(enumJson, play2Json).filter(!_.isEmpty).mkString("\n\n").indent(4)}
  }
}
$bindables
"""

    Seq(ServiceFileNames.toFile(form.service.namespace, form.service.organization.key, form.service.application.key, form.service.version, "Models", source, Some("Scala")))
  }
}
