package models.generator.javaAwsLambdaPojos

import lib.AbstractApiBuilderComments

case class ApiBuilderComments(override val version: String, override val userAgent: Option[String]) extends AbstractApiBuilderComments(version, userAgent) {

  val forClassFile: String = elements.mkString("", "\n","\n")

}
