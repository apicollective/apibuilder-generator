package models

import lib.Text._
import generator.{ScalaDatatype, ScalaModel}

case class Play2Json(
  serviceName: String,
  modelName: String,
  model: ScalaModel
) {

  def generate(): String = {
    readers + "\n\n" + writers
  }

  def readers(): String = {
    Seq(
      s"implicit def jsonReads${serviceName}$modelName: play.api.libs.json.Reads[$modelName] = {",
      fieldReaders().indent(2),
      s"}"
    ).mkString("\n")
  }

  def fieldReaders(): String = {
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
        serializations.head + s""".map { x => new $modelName(${field.name} = x) }"""
      }
      case fields => {
        Seq(
          "(",
          serializations.mkString(" and\n").indent(2),
          s")($modelName.apply _)"
        ).mkString("\n")
      }
    }
  }

  def writers(): String = {
    model.fields match {
      case field :: Nil => {
        Seq(
          s"implicit def jsonWrites${serviceName}$modelName: play.api.libs.json.Writes[$modelName] = new play.api.libs.json.Writes[$modelName] {",
          s"  def writes(x: $modelName) = play.api.libs.json.Json.obj(",
          s"""    "${field.originalName}" -> play.api.libs.json.Json.toJson(x.${field.name})""",
          "  )",
          "}"
        ).mkString("\n")
      }

      case fields => {
        Seq(
          s"implicit def jsonWrites${serviceName}$modelName: play.api.libs.json.Writes[$modelName] = {",
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
          s"  )(unlift($modelName.unapply _))",
          s"}"
        ).mkString("\n")
      }
    }
  }

}
