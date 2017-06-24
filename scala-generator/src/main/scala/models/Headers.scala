package scala.models

import scala.generator.ScalaUtil
import io.apibuilder.generator.v0.models.InvocationForm
import lib.VersionTag
import lib.Text._

case class Headers(
  form: InvocationForm
) {

  private[this] val versionMajor: Option[Int] = VersionTag(form.service.version).major

  private[this] val NamespaceName = "Namespace"
  private[this] val VersionMajorName = "VersionMajor"
  private[this] val VersionMajorHeaderName = "X-Apidoc-Version-Major"

  private[this] val constants = Seq(
    form.service.baseUrl.map { url => ("BaseUrl", url) },
    Some("Namespace", form.service.namespace),
    Some("UserAgent", form.userAgent.getOrElse("apidoc:play_2x_client:unknown")),
    Some("Version", form.service.version),
    versionMajor.map { major => (VersionMajorName, major.toString) }
  ).flatten

  val objectConstants: String = {
    Seq(
      "object Constants {",
      constants.map { pair =>
        if (pair._1 == VersionMajorName) {
          s"val ${pair._1} = ${pair._2.trim}"
        } else {
          s"val ${pair._1} = ${ScalaUtil.wrapInQuotes(pair._2.trim)}"
        }
      }.mkString("\n").indent(2),
      "}"
    ).mkString("\n\n")
  }

  val scala = Seq(
    Some("User-Agent" -> s"Constants.UserAgent"),
    Some("X-Apidoc-Version" -> s"Constants.Version"),
    versionMajor.map { major => VersionMajorHeaderName -> s"Constants.VersionMajor.toString" }
  ).flatten ++ form.service.headers.filter(!_.default.isEmpty).map { h =>
    (h.name -> ScalaUtil.wrapInQuotes(h.default.get))
  }

}
