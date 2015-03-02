package generator

import com.gilt.apidoc.spec.v0.models._
import lib.{Datatype, DatatypeResolver, Methods, Primitives, Text, Type, Kind}
import models.Container

case class ScalaService(
  val service: Service
) {
  val namespaces = Namespaces(service.namespace)

  private val scalaTypeResolver = ScalaTypeResolver(namespaces)

  val datatypeResolver = GeneratorUtil.datatypeResolver(service)

  val name = ScalaUtil.toClassName(service.name)
 
  def unionClassName(name: String) = namespaces.unions + "." + ScalaUtil.toClassName(name)
  def modelClassName(name: String) = namespaces.models + "." + ScalaUtil.toClassName(name)
  def enumClassName(name: String) = namespaces.enums + "." + ScalaUtil.toClassName(name)

  val models = service.models.sortWith { _.name < _.name }.map { new ScalaModel(this, _) }

  val enums = service.enums.sortWith { _.name < _.name }.map { new ScalaEnum(_) }

  val unions = service.unions.sortWith { _.name < _.name }.map { new ScalaUnion(this, _) }

  val resources = service.resources.map { r => new ScalaResource(this, r) }

  def scalaDatatype(
    t: Datatype
  ): ScalaDatatype = {
    scalaTypeResolver.scalaDatatype(t)
  }

  def unionsForModel(model: ScalaModel): Seq[ScalaUnion] = {
    unions.filter { u =>
      u.types.flatMap(_.model).contains(model)
    }
  }

  def unionsForEnum(enum: ScalaEnum): Seq[ScalaUnion] = {
    unions.filter { u =>
      u.types.flatMap(_.enum).contains(enum)
    }
  }

}

class ScalaUnion(val ssd: ScalaService, val union: Union) {

  val name: String = ScalaUtil.toClassName(union.name)

  val qualifiedName = ssd.unionClassName(name)

  val description: Option[String] = union.description

  // Include an undefined instance to nudge the developer to think
  // about what happens in the future when a new type is added to the
  // union type.
  val undefinedType = ScalaPrimitive.Model(ssd.namespaces.models, name + "UndefinedType")

  val types: Seq[ScalaUnionType] = union.types.map { ScalaUnionType(ssd, _) }

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
  originalName: String,
  datatype: ScalaDatatype,
  enum: Option[ScalaEnum] = None,
  model: Option[ScalaModel] = None
)

object ScalaUnionType {

  def apply(ssd: ScalaService, t: UnionType): ScalaUnionType = {
    val `type` = ssd.datatypeResolver.parse(t.`type`).getOrElse {
      sys.error(s"Could not parse type[${t.`type`}] for union type[$t]")
    }
    val dt:ScalaDatatype = ssd.scalaDatatype(`type`)
    dt.primitive match {
      case ScalaPrimitive.Enum(ns, name) => {
        val enum = ssd.enums.find(_.name == name).getOrElse {
          sys.error(s"UnionType[$t] Failed to find enum[$name]")
        }

        ScalaUnionType(t.`type`, dt, enum = Some(enum))
      }

      case ScalaPrimitive.Model(ns, name) => {
        val model = ssd.models.find(_.name == name).getOrElse {
          sys.error(s"UnionType[$t] Failed to find model[$name]")
        }

        ScalaUnionType(t.`type`, dt, model = Some(model))
      }
      case _ => {
        ScalaUnionType(t.`type`, dt)
      }
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

  val serverArgList: Option[String] = ScalaUtil.fieldsToArgList(fields.map(_.serverDefinition()))

}

class ScalaBody(ssd: ScalaService, val body: Body) {

  val `type`: Datatype = ssd.datatypeResolver.parse(body.`type`).getOrElse {
    sys.error(s"Could not parse type[${body.`type`}] for body[$body]")
  }

  val datatype = ssd.scalaDatatype(`type`)

  val multiple = `type` match {
    case Datatype.Singleton(_) => false
    case Datatype.List(_) | Datatype.Map(_) => true
  }

  val name: String = `type`.`type` match {
    case Type(Kind.Primitive, _) => {
      ScalaUtil.toDefaultClassName(multiple = multiple)
    }
    case Type(Kind.Model | Kind.Enum | Kind.Union, name) => {
      ScalaUtil.toClassName(name, multiple = multiple)
    }
  }

}

class ScalaEnum(val enum: Enum) {

  val name: String = ScalaUtil.toClassName(enum.name)

  val description: Option[String] = enum.description

  val values: Seq[ScalaEnumValue] = enum.values.map { new ScalaEnumValue(_) }

}

class ScalaEnumValue(value: EnumValue) {

  val originalName: String = value.name

  val name: String = ScalaUtil.toClassName(value.name)

  val description: Option[String] = value.description

}

class ScalaResource(ssd: ScalaService, val resource: Resource) {

  val plural: String = ScalaUtil.toClassName(resource.plural)

  val namespaces = ssd.namespaces

  val operations = resource.operations.map { new ScalaOperation(ssd, _, this)}

}

class ScalaOperation(val ssd: ScalaService, operation: Operation, resource: ScalaResource) {

  val method: Method = operation.method

  val path: String = operation.path

  val description: Option[String] = operation.description

  val body: Option[ScalaBody] = operation.body.map(new ScalaBody(ssd, _))

  val parameters: List[ScalaParameter] = {
    operation.parameters.toList.map { new ScalaParameter(ssd, _) }
  }

  lazy val pathParameters = parameters.filter { _.location == ParameterLocation.Path }

  lazy val queryParameters = parameters.filter { _.location == ParameterLocation.Query }

  lazy val formParameters = parameters.filter { _.location == ParameterLocation.Form }

  val name: String = GeneratorUtil.urlToMethodName(resource.plural, operation.method, path)

  val clientArgList: Option[String] = body match {
    case None => {
      ScalaUtil.fieldsToArgList(parameters.map(_.clientDefinition()))
    }
    case Some(body) => {
      val bodyVarName = ScalaUtil.toVariable(body.`type`)

      ScalaUtil.fieldsToArgList(
        parameters.filter(_.param.required).map(_.clientDefinition()) ++
        Seq(s"%s: %s".format(ScalaUtil.quoteNameIfKeyword(bodyVarName), body.datatype.name)) ++
        parameters.filter(!_.param.required).map(_.clientDefinition())
      )
    }
  }

  val responses: Seq[ScalaResponse] = operation.responses
    .sortWith { _.code < _.code }
    .map { new ScalaResponse(ssd, method, _) }

  lazy val resultType = responses.find(_.isSuccess).map(_.resultType).getOrElse("Unit")

}

class ScalaResponse(ssd: ScalaService, method: Method, response: Response) {

  val code: Int = response.code

  val `type`: Datatype = ssd.datatypeResolver.parse(response.`type`).getOrElse {
    sys.error(s"Could not parse type[${response.`type`}] for response[$response]")
  }

  val isOption = Container(`type`) match {
    case Container.Singleton => !Methods.isJsonDocumentMethod(method.toString)
    case Container.List | Container.Map => false
  }

  val isSuccess = code >= 200 && code < 300
  val isNotFound = code == 404

  val datatype = ssd.scalaDatatype(`type`)

  val isUnit = `type`.`type` == Type(Kind.Primitive, Primitives.Unit.toString)

  val resultType: String = datatype.name

  val errorVariableName = ScalaUtil.toVariable(`type`)

  val errorClassName = lib.Text.initCap(errorVariableName) + "Response"
}

class ScalaField(ssd: ScalaService, modelName: String, field: Field) {

  def name: String = ScalaUtil.quoteNameIfKeyword(Text.snakeToCamelCase(field.name))

  def originalName: String = field.name

  val `type`: Datatype = ssd.datatypeResolver.parse(field.`type`).getOrElse {
    sys.error(s"Could not parse type[${field.`type`}] for model[$modelName] field[$name]")
  }

  def datatype = ssd.scalaDatatype(`type`)

  def description: Option[String] = field.description

  /**
   * If there is a default, ensure it is only set server side otherwise
   * changing the default would have no impact on deployed clients
   */
  def isOption: Boolean = !field.required || field.default.nonEmpty

  def clientDefinition(varName: String = name): String = {
    datatype.clientDefinition(varName, isOption)
  }

  def serverDefinition(varName: String = name): String = {
    datatype.serverDefinition(varName, isOption)
  }
}

class ScalaParameter(ssd: ScalaService, val param: Parameter) {

  def name: String = ScalaUtil.toVariable(param.name)

  val `type`: Datatype = ssd.datatypeResolver.parse(param.`type`).getOrElse {
    sys.error(s"Could not parse type[${param.`type`}] for param[$param]")
  }

  def originalName: String = param.name

  def datatype = ssd.scalaDatatype(`type`)
  def description: String = param.description.getOrElse(name)

  def default = param.default

  /**
   * If there is a default, ensure it is only set server side otherwise
   * changing the default would have no impact on deployed clients
   */
  def isOption: Boolean = !param.required || param.default.nonEmpty

  def clientDefinition(varName: String = name): String = {
    datatype.clientDefinition(varName, isOption)
  }

  def serverDefinition(varName: String = name): String = {
    datatype.serverDefinition(varName, isOption)
  }

  def location = param.location
}
