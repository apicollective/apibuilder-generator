package models.generator.android

import lib.AbstractApiBuilderComments

/**
 *
 * Author: jkenny
 * Date: 28/05/2015
 */
case class ApiBuilderComments(override val version: String, override val userAgent: Option[String]) extends AbstractApiBuilderComments(version, userAgent) {

  val forClassFile: String = elements.mkString("", "\n","\n")

}
