package models.generator.android

import lib.Constants

/**
 *
 * Author: jkenny
 * Date: 28/05/2015
 */
class ApidocComments(version: String, userAgent: Option[String]) {

  private val elements = Seq(
    Some(s"Generated by API Builder - ${Constants.ApiBuilderUrl}"),
    Some(s"Service version: $version"),
    userAgent
  ).flatten

  val forClassFile: String = elements.mkString("", "\n","\n")

}
