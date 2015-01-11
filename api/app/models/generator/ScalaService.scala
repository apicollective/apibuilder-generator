package generator

import com.gilt.apidocspec.models._
import lib.{Datatype, DatatypeResolver, Methods, Primitives, Text, Type, TypeKind}
import models.Container

case class ScalaService(
  service: Service
) {
  val namespace = service.namespace

  val modelNamespace = s"$namespace.models"
  val enumNamespace = modelNamespace

  private val scalaTypeResolver = ScalaTypeResolver(
    modelNamespace = modelNamespace,
    enumNamespace = enumNamespace
  )


  val datatypeResolver = DatatypeResolver(
    enumNames = service.enums.map(_.name) ++ service.imports.flatMap { imp =>
      imp.enums.map(name => s"${imp.namespace}.enums.${name}")
    },
    modelNames = service.models.map(_.name) ++ service.imports.flatMap { imp =>
      imp.models.map(name => s"${imp.namespace}.models.${name}")
    }
  )

  val name = ScalaUtil.toClassName(service.name)
 
  def modelClassName(name: String) = modelNamespace + "." + ScalaUtil.toClassName(name)
  def enumClassName(name: String) = enumNamespace + "." + ScalaUtil.toClassName(name)
  // TODO: End make these private

  val models = service.models.map { new ScalaModel(this, _) }

  val enums = service.enums.map { new ScalaEnum(_) }

  val namespacePrivate = namespace.split("\\.").last

  val defaultHeaders: Seq[ScalaHeader] = {
    service.headers.flatMap { h => h.default.map { default => ScalaHeader(h.name, default) } }
  }

  val resources = service.resources.map { r =>
    val sm = models.find(_.model.name == r.model.name).getOrElse {
      sys.error("Could not find scala model for resource: " + r)
    }
    new ScalaResource(this, r, sm)
  }

  def scalaDatatype(
    t: Datatype
  ): ScalaDatatype = {
    scalaTypeResolver.scalaDatatype(t)
  }

}

case class ScalaHeader(name: String, value: String) {
  val quotedValue = s""""$value""""
}


class ScalaModel(val ssd: ScalaService, val model: Model) {

  val originalName: String = model.name

  val name: String = ScalaUtil.toClassName(model.name)

  val plural: String = ScalaUtil.toClassName(model.plural)

  val description: Option[String] = model.description

  val fields = model.fields.map { f => new ScalaField(ssd, this.name, f) }.toList

  val argList: Option[String] = ScalaUtil.fieldsToArgList(fields.map(_.definition()))

}

class ScalaBody(ssd: ScalaService, val body: Body) {

  val `type`: Datatype = ssd.datatypeResolver.parse(body.`type`).getOrElse {
    sys.error(s"Could not parse type[${body.`type`}] for body[$body]")
  }

  val datatype = ssd.scalaDatatype(`type`)

  val multiple = `type` match {
    case Datatype.Singleton(_) | Datatype.Option(_) => false
    case Datatype.List(_) | Datatype.Map(_) => true
  }

  val name: String = `type`.types.toList match {
    case (single :: Nil) => {
      single match {
        case Type(TypeKind.Primitive, _) => {
          ScalaUtil.toDefaultClassName(multiple = multiple)
        }
        case Type(TypeKind.Model, name) => {
          ScalaUtil.toClassName(name, multiple = multiple)
        }
        case Type(TypeKind.Enum, name) => {
          ScalaUtil.toClassName(name, multiple = multiple)
        }
      }
    }
    case (multiple) => {
      sys.error("TODO: UNION TYPE")
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

class ScalaResource(ssd: ScalaService, val resource: Resource, val model: ScalaModel) {

  val namespace: String = ssd.namespace

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

  val name: String = GeneratorUtil.urlToMethodName(resource.model.plural, operation.method, path)

  val argList: Option[String] = body match {
    case None => {
      ScalaUtil.fieldsToArgList(parameters.map(_.definition()))
    }
    case Some(body) => {
      val varName = ScalaUtil.toVariable(body.`type`)

      Some(
        Seq(
          Some(s"%s: %s".format(ScalaUtil.quoteNameIfKeyword(varName), body.datatype.name)),
          ScalaUtil.fieldsToArgList(parameters.map(_.definition()))
        ).flatten.mkString(",")
      )
    }
  }

  private def bodyClassArg(
    name: String,
    multiple: Boolean
  ): String = {
    val baseClassName = ssd.modelClassName(name)
    val className = if (multiple) {
      s"Seq[$baseClassName]"
    } else {
      baseClassName
    }

    Seq(
      Some(s"${ScalaUtil.toVariable(name, multiple)}: $className"),
      ScalaUtil.fieldsToArgList(parameters.map(_.definition()))
    ).flatten.mkString(",")
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
    case Container.Singleton | Container.Option => !Methods.isJsonDocumentMethod(method.toString)
    case Container.List | Container.Map => false
  }

  val isSuccess = code >= 200 && code < 300
  val isNotFound = code == 404

  val datatype = ssd.scalaDatatype(`type`)

  val isUnit = `type`.types.toList.forall( _ == Type(TypeKind.Primitive, Primitives.Unit.toString) )

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

  def definition(varName: String = name): String = {
    datatype.definition(varName, isOption)
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

  def definition(varName: String = name): String = {
    datatype.definition(varName, isOption)
  }

  def location = param.location
}
