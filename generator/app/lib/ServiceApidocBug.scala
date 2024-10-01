package lib

import play.api.libs.json.{JsArray, JsObject, JsDefined, JsValue, Json}

/**
 * Version 0.4.28 of apibuilder-validation has an invalid Service spec definition which makes
 * the apidoc field required. This dependency is pulled in by apibuilder-graphql. Once we can
 * migrate the generator to scala3 we can pull in the latest dependencies. Until then to keep
 * things working, we inject a default "apidoc" node where not specified.
 */
object ServiceApidocBug {
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

  private def rewriteObject(js: JsObject): JsObject = {
    rewriteImportedServices(
      rewriteService(js)
    )
  }

  private def rewriteService(js: JsObject): JsObject = {
    js \ "service" match {
      case JsDefined(svc: JsObject) => {
        js ++ Json.obj("service" -> maybeAddApidoc(svc))
      }
      case _ => {
        println(s"rewriteService: Skipping service as field not found")
        js
      }
    }
  }

  private def rewriteImportedServices(js: JsObject): JsObject = {
    js \ "imported_services" match {
      case JsDefined(svc: JsArray) => {
        js ++ Json.obj(
          "imported_services" -> JsArray(
            svc.value.toSeq.map {
              case o: JsObject => maybeAddApidoc(o)
              case o => o
            }
          )
        )
      }
      case _ => js
    }
  }

  private def maybeAddApidoc(js: JsObject): JsObject = {
    if (js.fields.contains("apidoc")) {
      js
    } else {
      println("Adding default apidoc field")
      js ++ DefaultApiDoc
    }
  }
}
