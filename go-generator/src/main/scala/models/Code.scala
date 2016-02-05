package go.models

import com.bryzek.apidoc.spec.v0.models.{Enum, Model, Service, Union}
import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import lib.Datatype
import lib.generator.GeneratorUtil
import lib.Text
import lib.Text._

case class Code(form: InvocationForm) {

  private[this] val service = form.service
  private[this] val datatypeResolver = GeneratorUtil.datatypeResolver(service)

  def generate(): Option[String] = {
    Seq(service.models, service.enums, service.unions).flatten.isEmpty match {
      case true => {
        None
      }
      case false => {
        Some(
          Seq(
            s"package ${GoUtil.packageName(service.name)}",
            Headers(form).code,
            Seq(
              service.enums.map(generateEnum(_)),
              service.models.map(generateModel(_)),
              service.unions.map(generateUnion(_))
            ).flatten.map(_.trim).filter(!_.isEmpty).mkString("\n\n")
          ).mkString("\n\n")
        )
      }
    }
  }

  private[this] def generateEnum(enum: Enum): String = {
    Seq(
      s"type ${GoUtil.publicName(enum.name)} struct {",
      "TODO: Finish enum implementation",
      "}"
    ).mkString("\n")
  }

  private[this] def datatype(typeName: String, required: Boolean): Datatype = {
    datatypeResolver.parse(typeName, required).getOrElse {
      sys.error(s"Unknown datatype[$typeName]")
    }
  }

  private[this] def generateModel(model: Model): String = {
    Seq(
      s"type ${GoUtil.publicName(model.name)} struct {",
      model.fields.map { f =>
        GoUtil.publicName(f.name) + " " + GoType(datatype(f.`type`, f.required)).className
      }.mkString("\n").indent(2),
      "}\n"
    ).mkString("\n")
  }

  private[this] def generateUnion(union: Union): String = {
    Seq(
      s"type ${GoUtil.publicName(union.name)} struct {",
      union.types.map { typ =>
        GoUtil.publicName(typ.`type`) + " " + GoType(datatype(typ.`type`, true)).className
      }.mkString("\n").indent(2),
      "}\n"
    ).mkString("\n")
  }


}
