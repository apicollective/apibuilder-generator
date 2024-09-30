package lib

import play.api.libs.json.{JsObject, JsValue, Json}

/**
 * Version 0.4.28 of apibuilder-validation has an invalid Service spec definition which makes
 * the apidoc field required. This dependency is pulled in by apibuilder-graphql. Once we can
 * migrate the generator to scala3 we can pull in the latest dependencies. Until then to keep
 * things working, we inject a default "apidoc" node where not specified.
 */
object InvalidSpecBug {
  private val DefaultApiDoc = Json.obj(
    "apidoc" -> Json.obj(
      "version" -> "0.16.0"
    )
  )

  def rewrite(js: JsValue): JsValue = {
    js match {
      case o: JsObject => rewriteObject(o)
      case _ => js
    }
  }

  private val RequiredFields = Seq(
    "name", "organization", "application", "namespace", "version"
  )
  private def rewriteObject(js: JsObject): JsObject = {
    def hasField(name: String): Boolean = js.keys.contains(name)
    val n = RequiredFields.map { f =>
      if (hasField(f)) { 1 } else { 0 }
    }.sum
    if (n >= RequiredFields.length && !hasField("apidoc")) {
      println("Adding default apidoc field")
      js ++ DefaultApiDoc
    } else {
      js
    }
  }
}
