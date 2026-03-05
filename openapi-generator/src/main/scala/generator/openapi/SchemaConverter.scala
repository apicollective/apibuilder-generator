package generator.openapi

import io.apibuilder.spec.v0.{models => ab}
import io.apibuilder.validation.{ApiBuilderType, MultiService}
import sttp.apispec.{ExampleSingleValue, Schema, SchemaType}

import scala.collection.immutable.ListMap

object SchemaConverter {

  def convert(multiService: MultiService, resolver: TypeResolver): ListMap[String, Schema] = {
    val modelSchemas =
      multiService.allModels.map(m => TypeResolver.sanitizeName(m.qualified) -> convertModel(m, resolver))
    val enumSchemas = multiService.allEnums.map(e => TypeResolver.sanitizeName(e.qualified) -> convertEnum(e))
    val unionSchemas =
      multiService.allUnions.map(u => TypeResolver.sanitizeName(u.qualified) -> convertUnion(u, resolver))
    ListMap.from(modelSchemas ++ enumSchemas ++ unionSchemas)
  }

  private def convertModel(model: ApiBuilderType.Model, resolver: TypeResolver): Schema = {
    val m = model.model
    val requiredFields = m.fields.filter(_.required).map(_.name)
    val properties = ListMap.from(m.fields.map(f => f.name -> convertField(f, resolver)))
    Schema(
      `type` = Some(List(SchemaType.Object)),
      properties = properties,
      required = requiredFields.toList,
      description = m.description,
      deprecated = m.deprecation.map(_ => true),
    )
  }

  private def convertField(field: ab.Field, resolver: TypeResolver): Schema = {
    val base = resolver.toSchemaOrEmpty(field.`type`)
    applyFieldMetadata(base, field)
  }

  private def applyFieldMetadata(schema: Schema, field: ab.Field): Schema = {
    val fieldType = field.`type`

    schema.copy(
      description = field.description.orElse(schema.description),
      default = if (isStringType(fieldType)) field.default.map(d => ExampleSingleValue(d)) else None,
      examples = field.example.map(e => List(ExampleSingleValue(e))),
      deprecated = field.deprecation.map(_ => true).orElse(schema.deprecated),
      minimum = if (isNumericType(fieldType)) field.minimum.map(BigDecimal(_)) else schema.minimum,
      maximum = if (isNumericType(fieldType)) field.maximum.map(BigDecimal(_)) else schema.maximum,
      minLength = if (isStringType(fieldType)) field.minimum.map(_.toInt) else schema.minLength,
      maxLength = if (isStringType(fieldType)) field.maximum.map(_.toInt) else schema.maxLength,
      minItems = if (isArrayType(fieldType)) field.minimum.map(_.toInt) else schema.minItems,
      maxItems = if (isArrayType(fieldType)) field.maximum.map(_.toInt) else schema.maxItems,
    )
  }

  private def convertEnum(e: ApiBuilderType.Enum): Schema = {
    val en = e.`enum`
    val values = en.values.map { v =>
      ExampleSingleValue(v.value.getOrElse(v.name))
    }
    Schema(
      `type` = Some(List(SchemaType.String)),
      `enum` = Some(values.toList),
      description = en.description,
      deprecated = en.deprecation.map(_ => true),
    )
  }

  private def convertUnion(union: ApiBuilderType.Union, resolver: TypeResolver): Schema = {
    val u = union.union
    val memberRefs = u.types.map { ut =>
      val resolved = resolver.resolve(ut.`type`)
      resolved match {
        case ResolvedType.Ref(path) => Schema($ref = Some(path))
        case _ => Schema($ref = Some(s"#/components/schemas/${TypeResolver.sanitizeName(ut.`type`)}"))
      }
    }
    Schema(
      oneOf = memberRefs.toList,
      description = u.description,
      deprecated = u.deprecation.map(_ => true),
    )
  }

  private val NumericTypes: Set[String] = Set("integer", "long", "double", "decimal", "float")
  private val StringTypes: Set[String] = Set("string", "uuid", "date-iso8601", "date-time-iso8601")

  private def isNumericType(t: String): Boolean = NumericTypes.contains(t)
  private def isStringType(t: String): Boolean = StringTypes.contains(t)
  private def isArrayType(t: String): Boolean = t.startsWith("[")
}
