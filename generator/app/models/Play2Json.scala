package models

import lib.Text._
import generator.{ScalaDatatype, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion}

case class Play2Json(
  ssd: ScalaService
) {

  private case class ReadWrite(name: String)
  private val Reads = ReadWrite("Reads")
  private val Writes = ReadWrite("Writes")

  def generate(): String = {
    Seq(
      ssd.models.map(generateModel(_)).mkString("\n\n"),
      ssd.unions.map(generateUnion(_)).mkString("\n\n")
    ).filter(!_.trim.isEmpty).mkString("\n\n")
  }

  private[models] def generateModel(model: ScalaModel): String = {
    readers(model) + "\n\n" + writers(model)
  }

  private[models] def generateUnion(union: ScalaUnion): String = {
    readers(union) + "\n\n" + writers(union)
  }

  private[models] def readers(union: ScalaUnion): String = {
    Seq(
      s"${identifier(union.name, Reads)} = {",
      s"  (",
      union.types.map { scalaUnionType =>
        s"""(__ \\ "${scalaUnionType.originalName}").read[${scalaUnionType.className}].asInstanceOf[play.api.libs.json.Reads[${union.name}]]"""
      }.mkString("\norElse\n").indent(4),
      s"  )",
      s"}"
    ).mkString("\n")
  }

  def writers(union: ScalaUnion): String = {
    Seq(
      s"private object ${helperName(union.name)} {",
      union.types.flatMap(_.model).map { m => s"  import ${helperName(m.name)}._" }.mkString("\n"),
      "",
      s"  def writes(obj: ${union.name}) = {",
      "    obj match {",
      union.types.map { t => s"""case x: ${t.className} => play.api.libs.json.Json.obj("${t.originalName}" -> x)""" }.mkString("\n").indent(6),
      "    }",
      "  }",
      "}",
      "",
      s"${identifier(union.name, Writes)} = new play.api.libs.json.Writes[${union.name}] {",
      s"  def writes(obj: ${union.name}) = ${helperName(union.name)}.writes(obj)",
      "}"
    ).mkString("\n")
  }

  private[models] def readers(model: ScalaModel): String = {
    Seq(
      s"${identifier(model.name, Reads)} = {",
      fieldReaders(model).indent(2),
      s"}"
    ).mkString("\n")
  }

  private[models] def fieldReaders(model: ScalaModel): String = {
    val serializations = model.fields.map { field =>
      field.datatype match {
        case ScalaDatatype.List(types) => {
          val nilValue = field.datatype.nilValue
          s"""(__ \\ "${field.originalName}").readNullable[${field.datatype.name}].map(_.getOrElse($nilValue))"""
        }
        case ScalaDatatype.Map(types) => {
          val nilValue = field.datatype.nilValue
          s"""(__ \\ "${field.originalName}").readNullable[${field.datatype.name}].map(_.getOrElse($nilValue))"""
        }
        case ScalaDatatype.Option(types) => {
            s"""(__ \\ "${field.originalName}").readNullable[${field.datatype.name}]"""
        }
        case ScalaDatatype.Singleton(types) => {
          if (field.isOption) {
            s"""(__ \\ "${field.originalName}").readNullable[${field.datatype.name}]"""
          } else {
            s"""(__ \\ "${field.originalName}").read[${field.datatype.name}]"""
          }
        }
      }
    }

    model.fields match {
      case field :: Nil => {
        serializations.head + s""".map { x => new ${model.name}(${field.name} = x) }"""
      }
      case fields => {
        Seq(
          "(",
          serializations.mkString(" and\n").indent(2),
          s")(${model.name}.apply _)"
        ).mkString("\n")
      }
    }
  }

  private def helperName(name: String): String = {
    s"${name}_Helper"
  }

  private[models] def writers(model: ScalaModel): String = {
    ssd.unionsForModel(model) match {
      case Nil => internalWriters(model)
      case unions => {
        Seq(
          "/**",
          s" * Writers are private as this model is part of a union type: ${unions.map(_.name).mkString(", ")}",
          s" */",
          s"private object ${helperName(model.name)} {",
          s"  import play.api.libs.json.__",
          s"  import play.api.libs.functional.syntax._",
          "",
          internalWriters(model).indent(2),
          "",
          s"}"
        ).mkString("\n")
      }
    }
  }

  private def internalWriters(model: ScalaModel): String = {
    model.fields match {
      case field :: Nil => {
        Seq(
          s"${identifier(model.name, Writes)} = new play.api.libs.json.Writes[${model.name}] {",
          s"  def writes(x: ${model.name}) = play.api.libs.json.Json.obj(",
          s"""    "${field.originalName}" -> play.api.libs.json.Json.toJson(x.${field.name})""",
          "  )",
          "}"
        ).mkString("\n")
      }

      case fields => {
        Seq(
          s"${identifier(model.name, Writes)} = {",
          s"  (",
          model.fields.map { field =>
            if (field.isOption) {
              field.datatype match {
                case ScalaDatatype.List(_) | ScalaDatatype.Map(_) | ScalaDatatype.Option(_) => {
                  s"""(__ \\ "${field.originalName}").write[${field.datatype.name}]"""
                }

                case ScalaDatatype.Singleton(_) => {
                  s"""(__ \\ "${field.originalName}").write[scala.Option[${field.datatype.name}]]"""
                }

              }
            } else {
              s"""(__ \\ "${field.originalName}").write[${field.datatype.name}]"""
            }
          }.mkString(" and\n").indent(4),
          s"  )(unlift(${model.name}.unapply _))",
          s"}"
        ).mkString("\n")
      }
    }
  }

  private[models] def identifier(
    name: String,
    readWrite: ReadWrite
  ): String = {
    s"implicit def json${readWrite.name}${ssd.name}$name: play.api.libs.json.${readWrite.name}[$name]"
  }

}
