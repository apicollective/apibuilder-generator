package scala.generator

import io.apibuilder.spec.v0.models._

import scala.models.{Attributes, Util}
import lib.{Datatype, DatatypeResolver, Methods, Text}
import lib.generator.GeneratorUtil
import play.api.libs.json.JsString

import scala.generator.ScalaPrimitive.{DateIso8601, DateTimeIso8601, JsonObject, JsonValue}

object ScalaService {
  def apply(service: Service, config: Attributes = Attributes.PlayDefaultConfig) = new ScalaService(service, config)
}

class ScalaService(
  val service: Service,
  val attributes: Attributes = Attributes.PlayDefaultConfig,
) {
  val namespaces: Namespaces = Namespaces(service.namespace)

  private[this] val scalaTypeResolver = ScalaTypeResolver(namespaces)

  val datatypeResolver: DatatypeResolver = GeneratorUtil.datatypeResolver(service)

  val name: String = ScalaUtil.toClassName(service.name)

  def unionClassName(name: String): String = namespaces.unions + "." + ScalaUtil.toClassName(name)
  def modelClassName(name: String): String = namespaces.models + "." + ScalaUtil.toClassName(name)
  def enumClassName(name: String): String = namespaces.enums + "." + ScalaUtil.toClassName(name)

  val interfaces: Seq[ScalaInterface] = service.interfaces.sortWith { _.name < _.name }.map { new ScalaInterface(this, _) }

  val models: Seq[ScalaModel] = service.models.sortWith { _.name < _.name }.map { new ScalaModel(this, _) }

  val enums: Seq[ScalaEnum] = service.enums.sortWith { _.name < _.name }.map { new ScalaEnum(this, _) }

  val unions: Seq[ScalaUnion] = service.unions.sortWith { _.name < _.name }.map { new ScalaUnion(this, _) }

  val resources: Seq[ScalaResource] = service.resources.map { r => new ScalaResource(this, r) }

  def scalaDatatype(t: Datatype): ScalaDatatype = {
    def convertObjectType(sd: ScalaDatatype): ScalaDatatype = sd match {
      case _: JsonObject => attributes.jsonLib.jsonObjectType

      case _: JsonValue => attributes.jsonLib.jsonValueType

      case _: DateIso8601 => attributes.dateType.dataType

      case _: DateTimeIso8601 => attributes.dateTimeType.dataType

      case ScalaDatatype.List(t) => ScalaDatatype.List(convertObjectType(t))

      case ScalaDatatype.Map(t) => ScalaDatatype.Map(convertObjectType(t))

      case ScalaDatatype.Option(t) => ScalaDatatype.Option(convertObjectType(t))

      case o => o
    }

    convertObjectType(scalaTypeResolver.scalaDatatype(t))
  }

  def unionsForModel(model: ScalaModel): Seq[ScalaUnion] =
    unionAndTypesForModel(model).map { case (u, _) => u }

  def unionAndTypesForModel(model: ScalaModel): Seq[(ScalaUnion, ScalaUnionType)] =
    unions.flatMap { u =>
      u.types.find(_.model.map(_.fullName).contains(model.qualifiedName)).map(u -> _)
    }

  def unionsForEnum(enum: ScalaEnum): Seq[ScalaUnion] =
    unionsAndTypesForEnum(enum).map { case (u, _) => u }

  def unionsAndTypesForEnum(enum: ScalaEnum): Seq[(ScalaUnion, ScalaUnionType)] =
    unions.flatMap { u =>
      u.types.find(_.enum.map(_.fullName).contains(enum.qualifiedName)).map(u -> _)
    }

  def unionsForUnion(union: ScalaUnion): Seq[ScalaUnion] = {
    unions.filter { u =>
      u.types.flatMap(_.union.map(_.fullName)).contains(union.qualifiedName)
    }
  }

  def errorParsingType(datatype: String, description: String): String = {
    Seq(
      s"Could not parse type[$datatype] for $description.",
      Seq(
        "Available Enums:",
        datatypeResolver.enumNames.map { s => s"  - $s" }.mkString("\n"),
        "Available Unions:",
        datatypeResolver.unionNames.map { s => s"  - $s" }.mkString("\n"),
        "Available Models:",
        datatypeResolver.modelNames.map { s => s"  - $s" }.mkString("\n")
      ).mkString("\n")
    ).mkString("\n")
  }

}

class ScalaUnion(val ssd: ScalaService, val union: Union) {

  val originalName: String = union.name

  val name: String = ScalaUtil.toClassName(union.name)

  val qualifiedName: String = ssd.unionClassName(name)

  val interfaces: Seq[String] = union.interfaces.map(ScalaUtil.toClassName)

  val discriminator: Option[String] = union.discriminator

  val description: Option[String] = union.description

  // Include an undefined instance to nudge the developer to think
  // about what happens in the future when a new type is added to the
  // union type.
  val undefinedType = ScalaPrimitive.Model(ssd.namespaces, name + "UndefinedType")

  val types: Seq[ScalaUnionType] = union.types.map { ScalaUnionType(ssd, _) }

  val deprecation: Option[Deprecation] = union.deprecation

}

/*
 * @param shortName The original short name to use in identifying objects of this
 *        type in the json document
 * @param locallyQualifiedName If the union type is defined in the same namespace
 *        as the service, returns only the scala class name.
 * @param fullyQualifiedName Fully qualified type name of the specific class name
 *        for each type.
 */
case class ScalaUnionType(
  ssd: ScalaService,
  value: UnionType,
  datatype: ScalaDatatype,
  enum: Option[ScalaPrimitive.Enum] = None,
  model: Option[ScalaPrimitive.Model] = None,
  union: Option[ScalaPrimitive.Union] = None
) {

  val name: String = ScalaUtil.toClassName(value.`type`)

  val isDefault: Boolean = value.default.getOrElse(false)

  val qualifiedName: String = ssd.modelClassName(name)

  val description: Option[String] = value.description

  val deprecation: Option[Deprecation] = value.deprecation

  val discriminatorName: String = value.discriminatorValue.getOrElse(value.`type`)

}

object ScalaUnionType {

  def apply(ssd: ScalaService, t: UnionType): ScalaUnionType = {
    val `type` = ssd.datatypeResolver.parse(t.`type`).getOrElse {
      sys.error(ssd.errorParsingType(t.`type`, s"union type[$t]"))
    }
    val dt: ScalaDatatype = ssd.scalaDatatype(`type`)
    dt match {
      case enum: ScalaPrimitive.Enum => {
        ScalaUnionType(ssd, t, dt, enum = Some(enum))
      }

      case model: ScalaPrimitive.Model => {
        ScalaUnionType(ssd, t, dt, model = Some(model))
      }

      case union: ScalaPrimitive.Union => {
        ScalaUnionType(ssd, t, dt, union = Some(union))
      }

      case p: ScalaPrimitive => {
        ScalaUnionType(ssd, t, p)
      }
      case c: ScalaDatatype.Container => sys.error(s"illegal container type ${c} encountered in union ${t.`type`}")
    }
  }

  def typeName(union: ScalaUnion, unionType: ScalaUnionType): String = {
    unionType.datatype match {
      case p @ (ScalaPrimitive.Model(_, _) | ScalaPrimitive.Enum(_, _) | ScalaPrimitive.Union(_, _)) => {
        p.name
      }
      case p: ScalaPrimitive => PrimitiveWrapper.className(union, p)
      case c: ScalaDatatype.Container => sys.error(s"unsupported container type ${c} encountered in union ${union.name}")
    }
  }
}

case class ModelAndInterface(
  name: String,
  plural: String,
  description: _root_.scala.Option[String],
  deprecation: _root_.scala.Option[io.apibuilder.spec.v0.models.Deprecation],
  fields: Seq[io.apibuilder.spec.v0.models.Field],
  attributes: Seq[io.apibuilder.spec.v0.models.Attribute],
  interfaces: Seq[String],
)

abstract class ScalaModelAndInterface(
  ssd: ScalaService,
  model: ModelAndInterface,
) {

  val originalName: String = model.name

  val name: String = ScalaUtil.toClassName(model.name)

  val qualifiedName: String = ssd.modelClassName(name)

  val plural: String = ScalaUtil.toClassName(model.plural)

  val description: Option[String] = model.description

  val fields: List[ScalaField] = model.fields.map { f => new ScalaField(ssd, this.name, f) }.toList

  val deprecation: Option[Deprecation] = model.deprecation

  val interfaces: Seq[String] = model.interfaces.map(ScalaUtil.toClassName)
}

class ScalaInterface(val ssd: ScalaService, val interface: Interface) extends ScalaModelAndInterface(
  ssd,
  ModelAndInterface(
    name = interface.name,
    plural = interface.plural ,
    description = interface.description,
    deprecation = interface.deprecation,
    fields = interface.fields,
    attributes = interface.attributes,
    interfaces = Nil,
  )
) {
  val body: Option[String] = fields match {
    case Nil => None
    case _ => Some(
      fields.map { f =>
        "def " + f.definition(isOverride = false)
      }.mkString("\n")
    )
  }
}

class ScalaModel(val ssd: ScalaService, val model: Model) extends ScalaModelAndInterface(
  ssd,
  ModelAndInterface(
    name = model.name,
    plural = model.plural ,
    description = model.description,
    deprecation = model.deprecation,
    fields = model.fields,
    attributes = model.attributes,
    interfaces = model.interfaces,
  )
) {
  private[this] val scalaInterfaces: Seq[ScalaInterface] = model.interfaces.flatMap { i =>
    ssd.interfaces.find(_.interface.name == i)
  }
  private[this] val inheritedFieldNames: Set[String] = scalaInterfaces.flatMap(_.fields.map(_.name)).toSet

  val argList: Option[String] = ScalaUtil.fieldsToArgList(
    fields.map { f =>
      f.definition(isOverride = inheritedFieldNames.contains(f.name))
    }
  )
}

class ScalaBody(ssd: ScalaService, val body: Body) {

  val datatype: ScalaDatatype = {
    val t = ssd.datatypeResolver.parse(body.`type`).getOrElse {
      sys.error(ssd.errorParsingType(body.`type`, s"body[$body]"))
    }
    ssd.scalaDatatype(t)
  }

  val name: String = datatype.name

}

class ScalaEnum(val ssd: ScalaService, val enum: Enum) {

  val originalName: String = enum.name

  val name: String = ScalaUtil.toClassName(originalName)

  def datatype = ScalaPrimitive.Enum(ssd.namespaces, name)

  val qualifiedName: String = ssd.enumClassName(name)

  val description: Option[String] = enum.description

  val values: Seq[ScalaEnumValue] = enum.values.map { new ScalaEnumValue(_) }

  val deprecation: Option[Deprecation] = enum.deprecation

}

class ScalaEnumValue(value: EnumValue) {

  val serializedValue: String = value.value.getOrElse(value.name)

  val name: String = ScalaUtil.toClassName(value.name)

  val description: Option[String] = value.description

  val deprecation: Option[Deprecation] = value.deprecation

}

class ScalaResource(ssd: ScalaService, val resource: Resource) {

  val path: Option[String] = resource.path

  val plural: String = ScalaUtil.toClassName(resource.plural)

  val namespaces: Namespaces = ssd.namespaces

  val operations: Seq[ScalaOperation] = resource.operations.map { new ScalaOperation(ssd, _, this)}

  val deprecation: Option[Deprecation] = resource.deprecation

}

class ScalaOperation(val ssd: ScalaService, operation: Operation, resource: ScalaResource) {

  val method: Method = operation.method

  val path: String = operation.path

  val deprecation: Option[Deprecation] = operation.deprecation

  val description: Option[String] = operation.description

  val body: Option[ScalaBody] = operation.body.map(new ScalaBody(ssd, _))

  val parameters: List[ScalaParameter] = {
    operation.parameters.toList.map { new ScalaParameter(ssd, _) }
  }

  lazy val pathParameters: List[ScalaParameter] = parameters.filter { _.location == ParameterLocation.Path }

  lazy val queryParameters: List[ScalaParameter] = parameters.filter { _.location == ParameterLocation.Query }

  lazy val formParameters: List[ScalaParameter] = parameters.filter { _.location == ParameterLocation.Form }

  lazy val (headerParameters, nonHeaderParameters) = parameters.partition { _.location == ParameterLocation.Header }

  val name: String = GeneratorUtil.urlToMethodName(resource.path, resource.resource.operations.map(_.path), operation.method, path)

  def argList(additionalArgs: Seq[String] = Nil): Option[String] = body match {
    case None => {
      ScalaUtil.fieldsToArgList(nonHeaderParameters.map(_.definition()) ++ headerParameters.map(_.definition()) ++ additionalArgs)
    }

    case Some(b) => {
      val bodyVarName = b.datatype.toVariableName

      ScalaUtil.fieldsToArgList(
        nonHeaderParameters.filter(_.param.required).map(_.definition()) ++
        Seq(s"%s: %s".format(ScalaUtil.quoteNameIfKeyword(bodyVarName), b.datatype.name)) ++
        headerParameters.filter(_.param.required).map(_.definition()) ++
        nonHeaderParameters.filter(!_.param.required).map(_.definition()) ++
        headerParameters.filter(!_.param.required).map(_.definition()) ++
        additionalArgs
      )
    }
  }

  val responses: Seq[ScalaResponse] = operation.responses
    .sortBy { r => Util.responseCodeAsString(r.code) }
    .map { new ScalaResponse(ssd, method, _) }

  lazy val resultType: String = responses.find(_.isSuccess).map(_.resultType).getOrElse("Unit")

}

class ScalaResponse(ssd: ScalaService, method: Method, response: Response) {

  val code: ResponseCode = response.code

  val `type`: Datatype = ssd.datatypeResolver.parse(response.`type`).getOrElse {
    sys.error(ssd.errorParsingType(response.`type`, s"response[$response]"))
  }

  val isOption: Boolean = `type` match {
    case Datatype.Container.List(_) | Datatype.Container.Map(_) => false
    case _ => !Methods.isJsonDocumentMethod(method.toString)
  }

  val isSuccess: Boolean = response.code match {
    case ResponseCodeInt(value) => value >= 200 && value < 300
    case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
  }

  val isNotFound: Boolean = response.code match {
    case ResponseCodeInt(value) => value == 404
    case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
  }

  val datatype: ScalaDatatype = ssd.scalaDatatype(`type`)

  val isUnit: Boolean = `type` == Datatype.Primitive.Unit

  val resultType: String = datatype.name

  val (errorVariableName, errorClassName) = if (isUnit) {
    (None, "UnitResponse")
  } else {
    val variableName = datatype.toVariableName
    (
      Some(variableName),
      lib.Text.initCap(variableName) + "Response"
    )
  }
}

class ScalaField(ssd: ScalaService, modelName: String, field: Field) {

  def name: String = ScalaUtil.quoteNameIfKeyword(Text.snakeToCamelCase(field.name))

  def originalName: String = field.name

  val `type`: Datatype = ssd.datatypeResolver.parse(field.`type`, shouldModelConcreteType).getOrElse {
    sys.error(ssd.errorParsingType(field.`type`, s"model[$modelName] field[$name]"))
  }

  def deprecation: Option[Deprecation] = field.deprecation

  def datatype: ScalaDatatype = ssd.scalaDatatype(`type`)

  def description: Option[String] = field.description

  def default: Option[String] = field.default.map(ScalaUtil.scalaDefault(_, datatype))

  def definition(varName: String = name, isOverride: Boolean): String = {
    datatype.definition(varName, default, deprecation, isOverride = isOverride)
  }

  def limitation: ScalaField.Limitation = ScalaField.Limitation(field.minimum, field.maximum)

  /**
    * A Scala type can be modeled in one of two ways:
    *  - Wire Friendly, in which the model captures the optionality of data on the wire.
    *  - Developer Friendly, in which the model ignores optionality if defaults are in play.
    *
    * This materializes in the behavior of not-required fields with defaults.
    *
    * In a developer friendly model, such fields
    * are modeled as concrete types (String, List[T], JsObject) as the default implies that an omitted value will be
    * filled in when marshalling and/or unmarshalling the object, and thus never be null.
    *
    * In a wire friendly model, such fields are modeled wrapped in an Option. If omitted when creating the model, a default
    * will be applied, and a default will be applied when unmarshalling the object as well. The Option exists so that a
    * sender can choose to omit the data (rather than applying a sender-side default) and thus defer the defaulting
    * behavior to the server.
    *
    * The global default is developer friendly. Wire friendly behavior can be introduced to any field by adding an
    * attribute to the field's spec, <pre>"attributes": [{"name": "scala_generator", "value": {"model_hint": "optional"}}]</pre>
    */
  def shouldModelConcreteType: Boolean = {
    val wireFriendly = field.attributes.exists(a =>
      a.name == "scala_generator" &&
        a.value.fields.exists(
          f=> f._1 == "model_hint" && f._2 == JsString("optional")
        )
    )

    (field.required, field.default, wireFriendly) match {
      case (true,  _,       _)     => true
      case (false, None,    _)     => false
      case (false, Some(_), true)  => false
      case (false, Some(_), false) => true
    }
  }

  def shouldApplyDefaultOnRead: Boolean = !field.required && field.default.nonEmpty
}

object ScalaField {
  final case class Limitation(minimum: Option[Long], maximum: Option[Long])
}

class ScalaParameter(ssd: ScalaService, val param: Parameter) {

  def name: String = ScalaUtil.toVariable(param.name)

  val `type`: Datatype = ssd.datatypeResolver.parse(param.`type`, required).getOrElse {
    sys.error(ssd.errorParsingType(param.`type`, s"param[$param]"))
  }

  def originalName: String = param.name

  def asScalaVal: String = ScalaUtil.quoteNameIfKeyword(name)

  def datatype: ScalaDatatype = ssd.scalaDatatype(`type`)

  def description: String = param.description.getOrElse(name)

  def default: Option[String] = param.default.map(ScalaUtil.scalaDefault(_, datatype))

  def required: Boolean = param.required

  def definition(varName: String = name): String = {
    datatype.definition(varName, default, param.deprecation, isOverride = false)
  }

  def location: ParameterLocation = param.location
}
