package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm
import lib.VersionTag
import lib.Text._

case class ScalaHeaders(
  form: InvocationForm
) {

  private val versionMajor: Option[Int] = VersionTag(form.service.version).major

  private val VersionMajorName = "VersionMajor"

  private val constants = Seq(
    Some("UserAgent", form.userAgent.getOrElse("apidoc:play_2x_client:unknown")),
    Some("Version", form.service.version),
    versionMajor.map { major => (VersionMajorName, major.toString) }
  ).flatten

  val objectConstants: String = {
    Seq(
      "object Constants {",
      constants.map { pair =>
        if (pair._1 == VersionMajorName) {
          s"val ${pair._1} = ${pair._2}"
        } else {
          s"val ${pair._1} = ${ScalaUtil.wrapInQuotes(pair._2)}"
        }
      }.mkString("\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  val all = Seq(
    Some("User-Agent" -> "Constants.UserAgent"),
    Some("X-Apidoc-Version" -> "Constants.Version"),
    versionMajor.map { major => "X-Apidoc-Version-Major" -> "Constants.VersionMajor.toString" }
  ).flatten

}
