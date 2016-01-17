package scala.generator

import scala.models.Play2JsonCommon
import lib.Text._

case class ScalaEnums(
  ssd: ScalaService,
  enum: ScalaEnum
) {

  private[this] val unions = ssd.unionsForEnum(enum)
  private[this] val play2JsonCommon = Play2JsonCommon(ssd)
  private[this] val jsObjectWriterMethod = play2JsonCommon.toJsonObjectMethodName(ssd.namespaces, enum.name)
  private[this] val jsValueWriterMethod = play2JsonCommon.implicitWriterName(enum.name)
  private[this] val implicitWriter = play2JsonCommon.implicitWriter(enum.name, enum.qualifiedName, jsValueWriterMethod)

  def build(): String = {
    import lib.Text._
    Seq(
      enum.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
      s"sealed trait ${enum.name}" + ScalaUtil.extendsClause(unions.map(_.name)).map(s => s" $s").getOrElse(""),
      s"object ${enum.name} {",
      buildValues().indent(2),
      s"}"
    ).mkString("\n\n")
  }

  /**
    * Returns the implicits for json serialization, handling
    * conversion both from the string and object representations.
    */
  def buildJson(): String = {
    Seq(
      s"implicit val jsonReads${ssd.name}${enum.name} = new play.api.libs.json.Reads[${enum.qualifiedName}] {",
      Seq(
        s"def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[${enum.qualifiedName}] = {",
        Seq(
          "js match {",
          Seq(
            s"case v: play.api.libs.json.JsString => play.api.libs.json.JsSuccess(${enum.qualifiedName}(v.value))",
            "case _ => {",
            Seq(
              """(js \ "value").validate[String] match {""",
              Seq(
                s"case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(${enum.qualifiedName}(v))",
                "case err: play.api.libs.json.JsError => err"
              ).mkString("\n").indent(2),
              "}"
            ).mkString("\n").indent(2),
            "}"
          ).mkString("\n").indent(2),
          "}"
        ).mkString("\n").indent(2),
        "}"
      ).mkString("\n").indent(2),
      "}",
      "",
      s"def $jsValueWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  play.api.libs.json.JsString(obj.toString)""",
      s"}",
      "",
      s"def $jsObjectWriterMethod(obj: ${enum.qualifiedName}) = {",
      s"""  play.api.libs.json.Json.obj("${PrimitiveWrapper.FieldName}" -> play.api.libs.json.JsString(obj.toString))""",
      s"}",
      "",
      implicitWriter
    ).mkString("\n")
  }

  private def buildValues(): String = {
    enum.values.map { value => 
      Seq(
        value.description.map { desc => ScalaUtil.textToComment(desc) },
        Some(s"""case object ${value.name} extends ${enum.name} { override def toString = "${value.originalName}" }""")
      ).flatten.mkString("\n")
    }.mkString("\n") + "\n" +
    s"""
/**
 * UNDEFINED captures values that are sent either in error or
 * that were added by the server after this library was
 * generated. We want to make it easy and obvious for users of
 * this library to handle this case gracefully.
 *
 * We use all CAPS for the variable name to avoid collisions
 * with the camel cased values above.
 */
case class UNDEFINED(override val toString: String) extends ${enum.name}

/**
 * all returns a list of all the valid, known values. We use
 * lower case to avoid collisions with the camel cased values
 * above.
 */
""" +
    s"val all = Seq(" + enum.values.map(_.name).mkString(", ") + ")\n\n" +
    s"private[this]\n" +
    s"val byName = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
    s"def apply(value: String): ${enum.name} = fromString(value).getOrElse(UNDEFINED(value))\n\n" +
    s"def fromString(value: String): _root_.scala.Option[${enum.name}] = byName.get(value.toLowerCase)\n\n"
  }

}
