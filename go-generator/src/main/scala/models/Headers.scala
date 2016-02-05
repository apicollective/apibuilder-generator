package go.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
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
    Some("Namespace", form.service.namespace),
    Some("UserAgent", form.userAgent.getOrElse("apidoc:play_2x_client:unknown")),
    Some("Version", form.service.version),
    versionMajor.map { major => (VersionMajorName, major.toString) }
  ).flatten

  val code: String = {
    Seq(
      constants.map { pair =>
        val name = GoUtil.publicName(pair._1)
        if (pair._1 == VersionMajorName) {
          s"const $name := ${pair._2}"
        } else {
          s"const $name := ${GoUtil.wrapInQuotes(pair._2)}"
        }
      }.mkString("\n")
    ).mkString("\n\n")
  }

  val all = Seq(
    Some("User-Agent" -> s"Constants::USER_AGENT"),
    Some("X-Apidoc-Version" -> s"Constants::VERSION"),
    versionMajor.map { major => VersionMajorHeaderName -> s"Constants::VERSION_MAJOR" }
  ).flatten ++ form.service.headers.filter(!_.default.isEmpty).map { h =>
    (h.name -> GoUtil.wrapInQuotes(h.default.get))
  }

}
