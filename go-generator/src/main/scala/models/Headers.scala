package go.models

import com.bryzek.apidoc.generator.v0.models.InvocationForm
import lib.VersionTag
import lib.Text._

case class Headers(
  importBuilder: ImportBuilder,
  form: InvocationForm
) {

  private[this] val versionMajor: Option[Int] = VersionTag(form.service.version).major

  private[this] val PackageName = "Package"
  private[this] val VersionMajorName = "VersionMajor"
  private[this] val VersionMajorHeaderName = "X-Apidoc-Version-Major"

  private[this] val constants = Seq(
    form.service.baseUrl.map { url => ("BaseUrl" -> url) },
    Some("UserAgent", form.userAgent.getOrElse("apidoc:go_1_5_client:unknown")),
    Some("Version", form.service.version),
    versionMajor.map { major => (VersionMajorName, major.toString) }
  ).flatten

  def generate(): String = {
    constants.map { pair =>
      val name = GoUtil.publicName(pair._1)
      if (pair._1 == VersionMajorName) {
        s"const $name = ${pair._2}"
      } else {
        s"const $name = ${GoUtil.wrapInQuotes(pair._2)}"
        }
    }.mkString("\n")
  }

  def all() = Seq(
    Some("User-Agent" -> s"UserAgent"),
    Some("X-Apidoc-Version" -> s"Version"),
    versionMajor.map { _ =>
      VersionMajorHeaderName -> s"${importBuilder.ensureImport("strconv")}.Itoa($VersionMajorName)"
    }
  ).flatten ++ form.service.headers.filter(!_.default.isEmpty).map { h =>
    (h.name -> GoUtil.wrapInQuotes(h.default.get))
  }

}
