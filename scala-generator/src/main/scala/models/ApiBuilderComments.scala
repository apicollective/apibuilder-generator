package scala.models

import lib.AbstractApiBuilderComments

import scala.generator.ScalaUtil

/**
  * @param version e.g. 1.2.3 Used to inject headers on the full version and the major version number
  */
case class ApiBuilderComments(override val version: String, override val userAgent: Option[String]) extends AbstractApiBuilderComments(version, userAgent) {

  def toJavaString: String = ScalaUtil.textToComment(elements)

  def forPlayRoutes: String = elements.map(s => s"# $s").mkString("\n")

}
