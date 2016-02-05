package golang.models

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, Service, Union}
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import lib.Text
import lib.Text._

object GoClientGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val header = ApidocComments(form.service.version, form.userAgent).comments + "\n"

    generateCode(form.service) match {
      case None => {
        Left(Seq("No enums, models, or unions were found and thus no client was generated"))
      }
      case Some(code) => {
        Right(
          Seq(
            ServiceFileNames.toFile(
              form.service.namespace,
              form.service.organization.key,
              form.service.application.key,
              form.service.version,
              "Client",
              Headers(form).code ++ "\n\n" ++ code,
              Some("go")
            )
          )
        )
      }
    }
  }

  private[this] def generateCode(service: Service): Option[String] = {
    Seq(service.models, service.enums, service.unions).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        Some(
          Seq(
            s"package ${service.namespace} {",
            Seq(
              service.enums.map(generateEnum(_)),
              service.models.map(generateModel(_)),
              service.unions.map(generateUnion(_))
            ).flatten.map(_.trim).filter(!_.isEmpty).mkString("\n\n").indent(2),
            "}"
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateEnum(enum: Enum): String = {
    Seq(
      s"type ${enum.name} struct {",
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateModel(model: Model): String = {
    Seq(
      s"type ${model.name} struct {",
      "}\n"
    ).mkString("\n\n")
  }

  private[this] def generateUnion(union: Union): String = {
    Seq(
      s"type ${union.name} struct {",
      "}\n"
    ).mkString("\n\n")
  }


}
