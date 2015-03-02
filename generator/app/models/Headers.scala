package models

import generator.ScalaUtil
import com.gilt.apidoc.generator.v0.models.InvocationForm
import lib.VersionTag
import lib.Text._

case class Headers(
  form: InvocationForm
) {

  private val versionMajor: Option[Int] = VersionTag(form.service.version).major

  private val VersionMajorName = "VersionMajor"
  private val VersionMajorHeaderName = "X-Apidoc-Version-Major"

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

  val rubyModuleConstants: String = {
    Seq(
      "module Constants",
      constants.map { pair =>
        val name = RubyUtil.toConstant(pair._1)
        if (pair._1 == VersionMajorName) {
          s"$name = ${pair._2} unless defined?($name)"
        } else {
          s"$name = ${RubyUtil.wrapInQuotes(pair._2)} unless defined?($name)"
        }
      }.mkString("\n").indent(2),
      "end"
    ).mkString("\n\n")
  }

  val scala = Seq(
    Some("User-Agent" -> s"Constants.UserAgent"),
    Some("X-Apidoc-Version" -> s"Constants.Version"),
    versionMajor.map { major => VersionMajorHeaderName -> s"Constants.VersionMajor.toString" }
  ).flatten ++ form.service.headers.filter(!_.default.isEmpty).map { h =>
    (h.name -> ScalaUtil.wrapInQuotes(h.default.get))
  }

  val ruby = Seq(
    Some("User-Agent" -> s"Constants::USER_AGENT"),
    Some("X-Apidoc-Version" -> s"Constants::VERSION"),
    versionMajor.map { major => VersionMajorHeaderName -> s"Constants::VERSION_MAJOR" }
  ).flatten ++ form.service.headers.filter(!_.default.isEmpty).map { h =>
    (h.name -> RubyUtil.wrapInQuotes(h.default.get))
  }

}
