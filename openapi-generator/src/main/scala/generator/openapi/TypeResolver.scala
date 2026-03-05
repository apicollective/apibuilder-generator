package generator.openapi

import io.apibuilder.validation.{MultiService, ScalarType}
import sttp.apispec.{AnySchema, Schema, SchemaLike, SchemaType}

import scala.collection.immutable.ListMap

sealed trait ResolvedType
object ResolvedType {
  case class Inline(schema: Schema) extends ResolvedType
  case class Ref(path: String) extends ResolvedType
  case object NoSchema extends ResolvedType
}

class TypeResolver(multiService: MultiService) {

  import TypeResolver._

  private val nameToQualified: Map[String, String] =
    multiService.allTypes.map(t => t.name -> sanitizeName(t.qualified)).toMap

  def resolve(typeString: String): ResolvedType = {
    typeString match {
      case _ if ScalarType.fromName(typeString).contains(ScalarType.UnitType) =>
        ResolvedType.NoSchema

      case ListRx(inner) =>
        val itemSchema = toSchema(inner)
        ResolvedType.Inline(
          Schema(
            `type` = Some(List(SchemaType.Array)),
            items = itemSchema,
          ),
        )

      case MapRx(inner) =>
        val valueSchema: Option[SchemaLike] = toSchema(inner)
        ResolvedType.Inline(
          Schema(
            `type` = Some(List(SchemaType.Object)),
            additionalProperties = valueSchema,
          ),
        )

      case name =>
        Primitives.get(name) match {
          case Some(mapping) =>
            val schema = if (FreeformTypes.contains(name)) {
              Schema(
                `type` = Some(List(mapping.schemaType)),
                additionalProperties = Some(AnySchema.Anything),
              )
            } else {
              Schema(
                `type` = Some(List(mapping.schemaType)),
                format = mapping.format,
              )
            }
            ResolvedType.Inline(schema)
          case None =>
            val schemaKey = nameToQualified.getOrElse(name, sanitizeName(name))
            ResolvedType.Ref(s"#/components/schemas/$schemaKey")
        }
    }
  }

  def toSchema(typeString: String): Option[Schema] = {
    resolve(typeString) match {
      case ResolvedType.Inline(schema) => Some(schema)
      case ResolvedType.Ref(path) => Some(Schema($ref = Some(path)))
      case ResolvedType.NoSchema => None
    }
  }

  def toSchemaOrEmpty(typeString: String): Schema = {
    toSchema(typeString).getOrElse(Schema())
  }

  def responseContent(typeString: String): ListMap[String, sttp.apispec.openapi.MediaType] = {
    resolve(typeString) match {
      case ResolvedType.NoSchema => ListMap.empty
      case _ =>
        val schema = toSchemaOrEmpty(typeString)
        ListMap("application/json" -> sttp.apispec.openapi.MediaType(schema = Some(schema)))
    }
  }
}

object TypeResolver {

  private val ListRx = "^\\[(.*)\\]$".r
  private val MapRx = "^map\\[(.*)\\]$".r

  private case class PrimitiveMapping(
    schemaType: SchemaType,
    format: Option[String] = None,
  )

  private val Primitives: Map[String, PrimitiveMapping] = Map(
    ScalarType.StringType.name -> PrimitiveMapping(SchemaType.String),
    ScalarType.IntegerType.name -> PrimitiveMapping(SchemaType.Integer, Some("int32")),
    ScalarType.LongType.name -> PrimitiveMapping(SchemaType.Integer, Some("int64")),
    ScalarType.DoubleType.name -> PrimitiveMapping(SchemaType.Number, Some("double")),
    ScalarType.DecimalType.name -> PrimitiveMapping(SchemaType.Number),
    ScalarType.FloatType.name -> PrimitiveMapping(SchemaType.Number, Some("float")),
    ScalarType.BooleanType.name -> PrimitiveMapping(SchemaType.Boolean),
    ScalarType.UuidType.name -> PrimitiveMapping(SchemaType.String, Some("uuid")),
    ScalarType.DateIso8601Type.name -> PrimitiveMapping(SchemaType.String, Some("date")),
    ScalarType.DateTimeIso8601Type.name -> PrimitiveMapping(SchemaType.String, Some("date-time")),
    ScalarType.JsonType.name -> PrimitiveMapping(SchemaType.Object),
    ScalarType.ObjectType.name -> PrimitiveMapping(SchemaType.Object),
  )

  private val FreeformTypes: Set[String] = Set(ScalarType.JsonType.name, ScalarType.ObjectType.name)

  def sanitizeName(name: String): String =
    name.split("[._]").map(_.capitalize).mkString
}
