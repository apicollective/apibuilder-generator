package scala.generator

import com.bryzek.apidoc.spec.v0.models._
import lib.{Datatype, DatatypeResolver, Methods, Text}
import lib.generator.GeneratorUtil
import scala.models.Util

case class ScalaService(
  val service: Service
) {
  val namespaces = Namespaces(service.namespace)

  private[this] val scalaTypeResolver = ScalaTypeResolver(namespaces)

  val datatypeResolver = GeneratorUtil.datatypeResolver(service)

  val name = ScalaUtil.toClassName(service.name)

  def unionClassName(name: String) = namespaces.unions + "." + ScalaUtil.toClassName(name)
  def modelClassName(name: String) = namespaces.models + "." + ScalaUtil.toClassName(name)
  def enumClassName(name: String) = namespaces.enums + "." + ScalaUtil.toClassName(name)

  val models = service.models.sortWith { _.name < _.name }.map { new ScalaModel(this, _) }

  val enums = service.enums.sortWith { _.name < _.name }.map { new ScalaEnum(this, _) }

  val unions = service.unions.sortWith { _.name < _.name }.map { new ScalaUnion(this, _) }

  val resources = service.resources.map { r => new ScalaResource(this, r) }

  def scalaDatatype(t: Datatype): ScalaDatatype = {
    scalaTypeResolver.scalaDatatype(t)
  }

  def unionsForModel(model: ScalaModel): Seq[ScalaUnion] = {
    unions.filter { u =>
      u.types.flatMap(_.model.map(_.fullName)).contains(model.qualifiedName)
    }
  }

  def unionsForEnum(enum: ScalaEnum): Seq[ScalaUnion] = {
    unions.filter { u =>
      u.types.flatMap(_.enum.map(_.fullName)).contains(enum.qualifiedName)
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

  val name: String = ScalaUtil.toClassName(union.name)

  val qualifiedName = ssd.unionClassName(name)

  val discriminator: Option[String] = union.discriminator

  val description: Option[String] = union.description

  // Include an undefined instance to nudge the developer to think
  // about what happens in the future when a new type is added to the
  // union type.
  val undefinedType = ScalaPrimitive.Model(ssd.namespaces, name + "UndefinedType")

  val types: Seq[ScalaUnionType] = union.types.map { ScalaUnionType(ssd, union, _) }

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
  val ssd: ScalaService,
  originalName: String,
  datatype: ScalaDatatype,
  enum: Option[ScalaPrimitive.Enum] = None,
  model: Option[ScalaPrimitive.Model] = None
) {

  val name: String = ScalaUtil.toClassName(originalName)

  val qualifiedName = ssd.modelClassName(name)

}

object ScalaUnionType {

  def apply(ssd: ScalaService, union: Union, t: UnionType): ScalaUnionType = {
    val `type` = ssd.datatypeResolver.parse(t.`type`, true).getOrElse {
      sys.error(ssd.errorParsingType(t.`type`, s"union type[$t]"))
    }
    val dt: ScalaDatatype = ssd.scalaDatatype(`type`)
    dt match {
      case enum: ScalaPrimitive.Enum => {
        ScalaUnionType(ssd, t.`type`, dt, enum = Some(enum))
      }

      case model: ScalaPrimitive.Model => {
        ScalaUnionType(ssd, t.`type`, dt, model = Some(model))
      }
      case p: ScalaPrimitive => {
        ScalaUnionType(ssd, t.`type`, p)
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

class ScalaModel(val ssd: ScalaService, val model: Model) {

  val originalName: String = model.name

  val name: String = ScalaUtil.toClassName(model.name)

  val qualifiedName: String = ssd.modelClassName(name)

  val plural: String = ScalaUtil.toClassName(model.plural)

  val description: Option[String] = model.description

  val fields = model.fields.map { f => new ScalaField(ssd, this.name, f) }.toList

  val argList: Option[String] = ScalaUtil.fieldsToArgList(fields.map(_.definition()))

  val deprecation: Option[Deprecation] = model.deprecation

}

class ScalaBody(ssd: ScalaService, val body: Body) {

  val datatype = {
    val t = ssd.datatypeResolver.parse(body.`type`, true).getOrElse {
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

  val qualifiedName = ssd.enumClassName(name)

  val description: Option[String] = enum.description

  val values: Seq[ScalaEnumValue] = enum.values.map { new ScalaEnumValue(_) }

  val deprecation: Option[Deprecation] = enum.deprecation

}

class ScalaEnumValue(value: EnumValue) {

  val originalName: String = value.name

  val name: String = ScalaUtil.toClassName(value.name)

  val description: Option[String] = value.description

  val deprecation: Option[Deprecation] = value.deprecation

}

class ScalaResource(ssd: ScalaService, val resource: Resource) {

  val path = resource.path

  val plural: String = ScalaUtil.toClassName(resource.plural)

  val namespaces = ssd.namespaces

  val operations = resource.operations.map { new ScalaOperation(ssd, _, this)}

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

  lazy val pathParameters = parameters.filter { _.location == ParameterLocation.Path }

  lazy val queryParameters = parameters.filter { _.location == ParameterLocation.Query }

  lazy val formParameters = parameters.filter { _.location == ParameterLocation.Form }

  lazy val (headerParameters, nonHeaderParameters) = parameters.partition { _.location == ParameterLocation.Header }

  val name: String = GeneratorUtil.urlToMethodName(resource.path, resource.resource.operations.map(_.path), operation.method, path)

  def argList(addlArgs: Seq[String] = Nil): Option[String] = body match {
    case None => {
      ScalaUtil.fieldsToArgList(nonHeaderParameters.map(_.definition()) ++ headerParameters.map(_.definition()) ++ addlArgs)
    }

    case Some(body) => {
      val bodyVarName = body.datatype.toVariableName

      ScalaUtil.fieldsToArgList(
        nonHeaderParameters.filter(_.param.required).map(_.definition()) ++
        Seq(s"%s: %s".format(ScalaUtil.quoteNameIfKeyword(bodyVarName), body.datatype.name)) ++
        nonHeaderParameters.filter(!_.param.required).map(_.definition()) ++
        headerParameters.map(_.definition()) ++
        addlArgs
      )
    }
  }

  val responses: Seq[ScalaResponse] = operation.responses
    .sortBy { r => Util.responseCodeAsString(r.code) }
    .map { new ScalaResponse(ssd, method, _) }

  lazy val resultType = responses.find(_.isSuccess).map(_.resultType).getOrElse("Unit")

}

class ScalaResponse(ssd: ScalaService, method: Method, response: Response) {

  val code: ResponseCode = response.code

  val `type`: Datatype = ssd.datatypeResolver.parse(response.`type`, true).getOrElse {
    sys.error(ssd.errorParsingType(response.`type`, s"response[$response]"))
  }

  val isOption = `type` match {
    case Datatype.Container.List(_) | Datatype.Container.Map(_) => false
    case _ => !Methods.isJsonDocumentMethod(method.toString)
  }

  val isSuccess = response.code match {
    case ResponseCodeInt(value) => value >= 200 && value < 300
    case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
  }

  val isNotFound = response.code match {
    case ResponseCodeInt(value) => value == 404
    case ResponseCodeOption.Default | ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
  }

  val datatype = ssd.scalaDatatype(`type`)

  val isUnit = `type` == Datatype.Primitive.Unit

  val resultType: String = datatype.name

  val (errorVariableName, errorClassName) = isUnit match {
    case true => {
      (None, "UnitResponse")
    }
    case false => {
      val variableName = datatype.toVariableName
      (
        Some(variableName),
        lib.Text.initCap(variableName) + "Response"
      )
    }
  }
}

class ScalaField(ssd: ScalaService, modelName: String, field: Field) {

  def name: String = ScalaUtil.quoteNameIfKeyword(Text.snakeToCamelCase(field.name))

  def originalName: String = field.name

  val `type`: Datatype = ssd.datatypeResolver.parse(field.`type`, required).getOrElse {
    sys.error(ssd.errorParsingType(field.`type`, s"model[$modelName] field[$name]"))
  }

  def datatype = ssd.scalaDatatype(`type`)

  def description: Option[String] = field.description

  def required = field.required

  def default = field.default.map(ScalaUtil.scalaDefault(_, datatype))

  def definition(varName: String = name): String = {
    datatype.definition(varName, default, field.deprecation)
  }
}

class ScalaParameter(ssd: ScalaService, val param: Parameter) {

  def name: String = ScalaUtil.toVariable(param.name)

  val `type`: Datatype = ssd.datatypeResolver.parse(param.`type`, required).getOrElse {
    sys.error(ssd.errorParsingType(param.`type`, s"param[$param]"))
  }

  def originalName: String = param.name

  def datatype = ssd.scalaDatatype(`type`)

  def description: String = param.description.getOrElse(name)

  def default = param.default.map(ScalaUtil.scalaDefault(_, datatype))

  def required = param.required

  def definition(varName: String = name): String = {
    datatype.definition(varName, default, param.deprecation)
  }

  def location = param.location
}
