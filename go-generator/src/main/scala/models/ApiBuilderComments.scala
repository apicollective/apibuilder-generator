package go.models

import lib.AbstractApiBuilderComments

/**
  * @param version e.g. 1.2.3 Used to inject headers on the full version and the major version number
  */
case class ApiBuilderComments(override val version: String, override val userAgent: Option[String]) extends AbstractApiBuilderComments(version, userAgent) {

  val comments: String = GoUtil.textToComment(elements)

}
