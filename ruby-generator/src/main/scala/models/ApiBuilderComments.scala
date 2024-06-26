package ruby.models

import lib.{AbstractApiBuilderComments, Constants}

/**
  * @param version e.g. 1.2.3 Used to inject headers on the full version and the major version number
  */
case class ApiBuilderComments(override val version: String, override val userAgent: Option[String]) extends AbstractApiBuilderComments(version, userAgent) {

  def toRubyString: String = RubyUtil.textToComment(elements)

}
