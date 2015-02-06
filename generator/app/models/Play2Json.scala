package models

import lib.Text._
import generator.{ScalaDatatype, ScalaModel, ScalaPrimitive, ScalaService, ScalaUnion}

case class Play2Json(
  ssd: ScalaService
) {

  private case class ReadWrite(name: String)
  private val Reads = ReadWrite("Reads")
  private val Writes = ReadWrite("Writes")

  case class GeneratedCode(packages: String, implicits: String)

  def generate(): GeneratedCode = {
    val modelsWithUnions: Seq[(ScalaModel, Seq[ScalaUnion])] = ssd.models.map { m => (m, ssd.unionsForModel(m)) }

    GeneratedCode(
      packages = Seq(
        modelsWithUnions.filter(!_._2.isEmpty).map(m => readersAndWriters(m._1, m._2)).mkString("\n\n"),
        ssd.unions.map(unionObject(_)).mkString("\n\n")
      ).filter(!_.trim.isEmpty).mkString("\n\n"),

      implicits = Seq(
        modelsWithUnions.filter(_._2.isEmpty).map(m => readersAndWriters(m._1, Nil)).mkString("\n\n"),
        ssd.unions.map(readers(_)).mkString("\n\n"),
        ssd.unions.map(writers(_)).mkString("\n\n")
      ).filter(!_.trim.isEmpty).mkString("\n\n")
    )
  }

  private[models] def readers(union: ScalaUnion): String = {
    s"${identifier(union.name, Reads)} = ${helperName(union.name)}.reads"
  }

  private def privateObject(name: String, body: Seq[String]): String = {
    Seq(
      s"private object ${helperName(name)} {",
      s"  import play.api.libs.json.__",
      s"  import play.api.libs.functional.syntax._",
      "",
      body.mkString("\n").indent(2),
      s"}"
    ).mkString("\n")
  }

  private[models] def unionObject(union: ScalaUnion): String = {
    privateObject(
      union.name,
      Seq(
        union.types.flatMap(_.model).map { m => s"import ${helperName(m.name)}._" }.mkString("\n"),
        s"def reads: play.api.libs.json.Reads[${union.name}] = {",
        s"  (",
        union.types.map { scalaUnionType =>
          s"""(__ \\ "${scalaUnionType.originalName}").read[${scalaUnionType.datatype.name}].asInstanceOf[play.api.libs.json.Reads[${union.name}]]"""
        }.mkString("\norElse\n").indent(4),
        s"  )",
        "}",
        "",
        s"def writes(obj: ${union.name}) = {",
        "  obj match {",
        union.types.map { t => s"""case x: ${t.datatype.name} => play.api.libs.json.Json.obj("${t.originalName}" -> x)""" }.mkString("\n").indent(6),
        "  }",
        "}"
      )
    )
  }

  private[models] def writers(union: ScalaUnion): String = {
    Seq(
      s"${identifier(union.name, Writes)} = new play.api.libs.json.Writes[${union.name}] {",
      s"  def writes(obj: ${union.name}) = ${helperName(union.name)}.writes(obj)",
      "}"
    ).mkString("\n")
  }


  private def helperName(name: String): String = {
    s"${name}_Helper"
  }

  private[models] def readersAndWriters(model: ScalaModel, unions: Seq[ScalaUnion]): String = {
    unions match {
      case Nil => readers(model) ++ writers(model)
      case unions => {
        s"// Private as this model is part of a union type: ${unions.map(_.name).mkString(", ")}\n" +
        privateObject(
          model.name,
          Seq(readers(model), "", writers(model))
        )
      }
    }
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

  private[models] def writers(model: ScalaModel): String = {
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
