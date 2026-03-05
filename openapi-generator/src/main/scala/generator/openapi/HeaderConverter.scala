package generator.openapi

import io.apibuilder.spec.v0.{models => ab}
import sttp.apispec.SecurityScheme

import scala.collection.immutable.ListMap

object HeaderConverter {

  case class Result(
    securitySchemes: ListMap[String, Either[sttp.apispec.openapi.Reference, SecurityScheme]],
    globalSecurity: List[ListMap[String, Vector[String]]],
  )

  def convert(headers: Seq[ab.Header]): Result = {
    if (headers.isEmpty) return Result(ListMap.empty, Nil)

    val schemes = headers.map { header =>
      val schemeName = schemeNameFor(header.name)
      val scheme = if (header.name.equalsIgnoreCase("Authorization")) {
        SecurityScheme(
          `type` = "http",
          scheme = Some("bearer"),
          description = header.description,
        )
      } else {
        SecurityScheme(
          `type` = "apiKey",
          name = Some(header.name),
          in = Some("header"),
          description = header.description,
        )
      }
      schemeName -> Right(scheme)
    }

    val security = headers.map { header =>
      ListMap(schemeNameFor(header.name) -> Vector.empty[String])
    }

    Result(
      securitySchemes = ListMap.from(schemes),
      globalSecurity = security.toList,
    )
  }

  private def schemeNameFor(headerName: String): String = {
    if (headerName.equalsIgnoreCase("Authorization")) "bearerAuth"
    else headerName.toLowerCase.replaceAll("[^a-z0-9]+", "_").replaceAll("^_|_$", "") + "_header"
  }
}
