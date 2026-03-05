package generator.openapi

import sttp.apispec.openapi.OpenAPI

object OutputWriter {

  def toJson(openApi: OpenAPI, pretty: Boolean = false): String = {
    import sttp.apispec.openapi.circe_openapi_3_0_3._
    import io.circe.syntax._
    val printer =
      if (pretty) io.circe.Printer.spaces2.copy(dropNullValues = true)
      else io.circe.Printer.noSpaces.copy(dropNullValues = true)
    printer.print(openApi.asJson)
  }

  def toYaml(openApi: OpenAPI): String = {
    import sttp.apispec.openapi.circe.yaml._
    openApi.toYaml3_0_3
  }
}
