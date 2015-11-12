package models.generator.android


import lib.Text
import lib.generator.CodeGenerator

import com.squareup.javapoet._
import javax.lang.model.element.Modifier
import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonProcessingException, Version}
import com.bryzek.apidoc.spec.v0.models._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import java.util.Locale
import java.io.IOException
import com.bryzek.apidoc.spec.v0.models.Enum
import scala.Some
import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.Union
import com.bryzek.apidoc.generator.v0.models.File
import com.bryzek.apidoc.spec.v0.models.Model
import com.bryzek.apidoc.spec.v0.models.Resource
import com.bryzek.apidoc.spec.v0.models.Service


object AndroidClasses
  extends CodeGenerator
  with AndroidJavaUtil{

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def invoke(form: InvocationForm, addHeader: Boolean = false): Either[Seq[String], Seq[File]] = Right(generateCode(form, addHeader))

  private def generateCode(form: InvocationForm, addHeader: Boolean = true): Seq[File] = {
    val header =
      if (addHeader) Some(
        s"${form.service.version}\n${form.userAgent}"
      )
      else None

    new Generator(form.service, header).generateSourceFiles()
  }

  class Generator(service: Service, header: Option[String]) {

    private val nameSpace = makeNameSpace(service.namespace)
    private val modelsNameSpace = nameSpace + ".models"
    private val modelsDirectoryPath = createDirectoryPath(modelsNameSpace)

    private val sharedJacksonSpace = "com.gilt.android.jackson"
    private val sharedJacksonDirectoryPath = createDirectoryPath(sharedJacksonSpace)
    private val sharedObjectMapperClassName = "ApidocObjectMapper"

    val T = "$T" //this is a hack for substituting types, as "$" is used by scala to do string substitution, and $T is sued by javapoet to handle types

    def createDirectoryPath(namespace: String) = namespace.replace('.', '/')

    def generateSourceFiles() = {

      val generatedEnums = service.enums.map { generateEnum }

      val generatedModels = service.models.map { model =>
        generateModel(model, Seq.empty)
      }

      val generatedResources = service.resources.map { generateResource }

      val generatedObjectMapper = Seq(generateObjectMapper)

      generatedEnums ++
        generatedModels ++
        generatedObjectMapper ++
        generatedResources
    }


    def generateObjectMapper: File = {

      def pattern: FieldSpec.Builder = {
        FieldSpec.builder(classOf[String], "pattern", Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
          .initializer("\"yyyy-MM-dd'T'HH:mm:ss.SSSZ\"")
      }

      def formatter: FieldSpec.Builder = {
        FieldSpec.builder(classOf[DateTimeFormatter], "formatter", Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
          .initializer("$T.forPattern(pattern).withLocale($T.US).withZoneUTC()", classOf[DateTimeFormat], classOf[Locale])
      }

      def deserializer: TypeSpec.Builder = {

        val deserialize = MethodSpec.methodBuilder("deserialize").addAnnotation(classOf[Override]).addModifiers(Modifier.PUBLIC)
          .addException(classOf[IOException])
          .addException(classOf[JsonProcessingException])
          .returns(classOf[DateTime])
          .addParameter(classOf[JsonParser], "jsonParser")
          .addParameter(classOf[DeserializationContext], "ctxt")
          .addStatement("String value = jsonParser.getValueAsString()")
          .addStatement("return formatter.parseDateTime(value)")

        TypeSpec.anonymousClassBuilder("").superclass(classOf[JsonDeserializer[DateTime]])
          .addMethod(deserialize.build)
      }


      def serializer: TypeSpec.Builder = {

        val serialize = MethodSpec.methodBuilder("serialize").addAnnotation(classOf[Override]).addModifiers(Modifier.PUBLIC)
          .addException(classOf[IOException])
          .addException(classOf[JsonProcessingException])
          .addParameter(classOf[DateTime], "value")
          .addParameter(classOf[JsonGenerator], "jgen")
          .addParameter(classOf[SerializerProvider], "provider")
          .addStatement("jgen.writeString(value.toString(formatter))")

        TypeSpec.anonymousClassBuilder("").superclass(classOf[JsonSerializer[DateTime]])
          .addMethod(serialize.build)
      }

      val builder =
        TypeSpec.classBuilder(sharedObjectMapperClassName).superclass(classOf[ObjectMapper]).addModifiers(Modifier.PUBLIC)

      builder.addField(pattern.build)
      builder.addField(formatter.build)

      val modifierField = FieldSpec.builder(classOf[ObjectMapper], "MAPPER").addModifiers(Modifier.PRIVATE).addModifiers(Modifier.FINAL).addModifiers(Modifier.STATIC)
      builder.addField(modifierField.build)

      builder.addStaticBlock(CodeBlock.builder
        .addStatement("SimpleModule module = new $T(new $T(1, 0, 0, null, null, null))", classOf[SimpleModule], classOf[Version])
        .addStatement("module.addDeserializer($T.class, $L)", classOf[DateTime], deserializer.build)
        .addStatement("module.addSerializer($T.class, $L)", classOf[DateTime], serializer.build)
        .addStatement("MAPPER = new ObjectMapper()")
        .addStatement("MAPPER.setPropertyNamingStrategy($T.CAMEL_CASE_TO_LOWER_CASE_WITH_UNDERSCORES)", classOf[PropertyNamingStrategy])
        .addStatement("MAPPER.configure($T.FAIL_ON_UNKNOWN_PROPERTIES, false)", classOf[DeserializationFeature])
        .addStatement("MAPPER.registerModule(module)")
        .build)

      val getterBuilder = MethodSpec.methodBuilder("getInstance").addModifiers(Modifier.PUBLIC).addModifiers(Modifier.STATIC)
      getterBuilder.returns(classOf[ObjectMapper])
      getterBuilder.addStatement(s"return MAPPER")
      builder.addMethod(getterBuilder.build)

      File(s"${sharedObjectMapperClassName}.java", Some(sharedJacksonDirectoryPath), JavaFile.builder(sharedJacksonSpace, builder.build).build.toString)
    }

    def generateEnum(enum: Enum): File = {

      val className = toClassName(enum.name)

      val builder =
        TypeSpec.enumBuilder(className)
        .addModifiers(Modifier.PUBLIC)

      enum.description.map(builder.addJavadoc(_))

      enum.values.foreach(value => {
        val annotation = AnnotationSpec.builder(classOf[JsonProperty]).addMember("value","\""+value.name+"\"")
        builder.addEnumConstant(value.name.toUpperCase(), TypeSpec.anonymousClassBuilder("").addAnnotation(annotation.build()).build())
      })

      makeFile(className, builder)

    }


    def generateModel(model: Model, relatedUnions: Seq[Union]): File = {


      val className = toClassName(model.name)

      val builder =
        TypeSpec.classBuilder(className)
          .addModifiers(Modifier.PUBLIC)

      val jsonIgnorePropertiesAnnotation = AnnotationSpec.builder(classOf[JsonIgnoreProperties]).addMember("ignoreUnknown","true")
      builder.addAnnotation(jsonIgnorePropertiesAnnotation.build)

      model.description.map(builder.addJavadoc(_))

      val constructorWithParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)
      constructorWithParams.addAnnotation(classOf[JsonCreator])

      val equals = MethodSpec.methodBuilder("equals").addModifiers(Modifier.PUBLIC).addParameter(classOf[Object], "o").returns(classOf[Boolean]).addAnnotation(classOf[Override])
      equals.addStatement("if (this == o) return true")
      equals.addStatement("if (o == null || getClass() != o.getClass()) return false")
      equals.addStatement(s"${className} that = (${className}) o")

      val hashCode = MethodSpec.methodBuilder("hashCode").addModifiers(Modifier.PUBLIC).returns(classOf[Int]).addAnnotation(classOf[Override])
      hashCode.addStatement("int result = 0")

      model.fields.foreach(field => {

        val fieldSnakeCaseName = field.name
        val arrayParameter = isParameterArray(field.`type`)
        val fieldCamelCaseName = toParamName(fieldSnakeCaseName, true)

        val javaDataType = dataTypeFromField(field.`type`)

        val fieldBuilder = FieldSpec.builder(javaDataType, fieldCamelCaseName).addModifiers(Modifier.PRIVATE).addModifiers(Modifier.FINAL)
        builder.addField(fieldBuilder.build)

        val methodName = Text.snakeToCamelCase(s"get_${fieldSnakeCaseName}")

        val getterBuilder = MethodSpec.methodBuilder(methodName).addModifiers(Modifier.PUBLIC)
        getterBuilder.returns(javaDataType)
        getterBuilder.addStatement(s"return ${fieldCamelCaseName}")
        field.description.map(getterBuilder.addJavadoc(_))
        builder.addMethod(getterBuilder.build)

        val constructorParameter = ParameterSpec.builder(javaDataType, fieldCamelCaseName)
        val annotation = AnnotationSpec.builder(classOf[JsonProperty]).addMember("value","\""+fieldSnakeCaseName+"\"")
        constructorParameter.addAnnotation(annotation.build)
        constructorWithParams.addParameter(constructorParameter.build)
        constructorWithParams.addStatement("this.$N = $N", fieldCamelCaseName, fieldCamelCaseName)

        if (arrayParameter) {
          equals.addStatement(s"if (!${T}.equals(${fieldCamelCaseName}, that.${fieldCamelCaseName})) return false", classOf[java.util.Arrays])
        } else {
          equals.addStatement(s"if (${fieldCamelCaseName} != null ? !${fieldCamelCaseName}.equals(that.${fieldCamelCaseName}) : that.${fieldCamelCaseName} != null) return false")
        }


        hashCode.addStatement(s"result = 31 * result + (${fieldCamelCaseName} != null ? ${fieldCamelCaseName}.hashCode() : 0)")
      })

      equals.addStatement("return true")

      hashCode.addStatement("return result")

      builder.addMethod(constructorWithParams.build)
      builder.addMethod(equals.build)
      builder.addMethod(hashCode.build)

      val toJsonString = MethodSpec.methodBuilder("toJsonString").addModifiers(Modifier.PUBLIC).returns(classOf[String]).addException(classOf[JsonProcessingException])
      toJsonString.addStatement(s"return ${sharedJacksonSpace}.${sharedObjectMapperClassName}.getInstance().writeValueAsString(this)")
      builder.addMethod(toJsonString.build)

      //      val constructor = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)
      //      constructor.addParameter(classOf[String], "json")
      //      builder.addMethod(constructor.build)

      makeFile(className, builder)

    }

    def generateResource(resource: Resource): File = {

      val className = toClassName(resource.plural) + "Client"

      val builder =
        TypeSpec.interfaceBuilder(className)
          .addModifiers(Modifier.PUBLIC)


      resource.operations.foreach{operation =>

        val maybeAnnotationClass = operation.method match {
          case Method.Get => Some(classOf[retrofit.http.GET])
          case Method.Post => Some(classOf[retrofit.http.POST])
          case Method.Put => Some(classOf[retrofit.http.PUT])
          case Method.Patch => Some(classOf[retrofit.http.PATCH])
          case Method.Delete => Some(classOf[retrofit.http.DELETE])
          case Method.Head => Some(classOf[retrofit.http.HEAD])
          case Method.Connect => None
          case Method.Options => None
          case Method.Trace => None
          case _ => None
        }

        import RetrofitUtil._

        val retrofitPath = toRetrofitPath(operation.path)

        maybeAnnotationClass.map(annotationClass => {

          val methodAnnotation = AnnotationSpec.builder(annotationClass).addMember("value", "\"" + retrofitPath + "\"").build()

          val methodName =
            if (operation.path == "/")
              toParamName(operation.method.toString.toLowerCase, true)
            else
              toParamName(operation.method.toString.toLowerCase + "_" + operation.path.replaceAll("/", "_"), true)

          val method = MethodSpec.methodBuilder(methodName).addModifiers(Modifier.PUBLIC).addModifiers(Modifier.ABSTRACT)

          operation.description.map(description => {
            method.addJavadoc(description)
          })

          operation.deprecation.map(deprecation => {
            val deprecationAnnotation = AnnotationSpec.builder(classOf[Deprecated]).build
            method.addAnnotation(deprecationAnnotation)
            deprecation.description.map(description => {
                method.addJavadoc("\n@deprecated: "+description)
            })
          })

          method.addAnnotation(methodAnnotation)

          operation.body.map(body => {
            val bodyType = dataTypeFromField(body.`type`)

            val parameter = ParameterSpec.builder(bodyType, toParamName(body.`type`, true))
            val annotation = AnnotationSpec.builder(classOf[retrofit.http.Body]).build
            parameter.addAnnotation(annotation)
            method.addParameter(parameter.build())
          })

          operation.parameters.foreach(parameter => {

            val maybeAnnotationClass = parameter.location match {
              case ParameterLocation.Path => Some(classOf[retrofit.http.Path])
              case ParameterLocation.Query => Some(classOf[retrofit.http.Query])
              case ParameterLocation.Form => Some(classOf[retrofit.http.Query])
              case _ => None
            }

            maybeAnnotationClass.map(annotationClass => {
              val parameterType: TypeName = dataTypeFromField(parameter.`type`)
              val param = ParameterSpec.builder(parameterType, toParamName(parameter.name, true))
              val annotation = AnnotationSpec.builder(annotationClass).addMember("value", "\"" + parameter.name + "\"").build
              param.addAnnotation(annotation)
              method.addParameter(param.build)
            })
          })

          /*
            this is where it gets a little ugly with apidoc/retrofit mapping.
            apidoc says "map the response code to response type", for example:

            "responses": {
              "201": { "type": "checkout_session"},
              "400": {"type": "error_response"},
              "401": {"type": "error_response"},
              "422": {"type": "error_response"}
            }

            retrofit, on the other hand, treats codes 200-299 as success and others as failure

            I think in most cases we can find a single 200-299 result and map it as success, and for other
            codes clients can do special handling based on response codes (without understanding the response object)

           */

          val maybeSuccessfulResponse = operation.responses.find(response => {
            response.code.isInstanceOf[ResponseCodeInt] &&
              response.code.asInstanceOf[ResponseCodeInt].value >= 200 &&
              response.code.asInstanceOf[ResponseCodeInt].value < 299
          })

          maybeSuccessfulResponse.map(successfulResponse => {
            val returnType = dataTypeFromField(successfulResponse.`type`)

            //below, Void in "classOf[retrofit.Call[Void]" is ignored, what matters is returnType
            val callTypeName = ParameterizedTypeName.get(ClassName.get(classOf[retrofit.Call[Void]]), returnType)
            method.returns(callTypeName)
          })
          builder.addMethod(method.build)
        })
      }

      makeFile(className, builder)
    }

    private def toMap(cc: AnyRef): Map[String,Any] =
      (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
        f.setAccessible(true)
        a + (f.getName -> f.get(cc))
      }


    private def commentFromOpt(opt: Option[String]) = {
      opt.fold("") { s => textToComment(s) + "\n" }
    }

    def camelToUnderscores(name: String): String = "[A-Z\\d]".r.replaceAllIn(name, {m =>
      "_" + m.group(0).toLowerCase()
    })


    def underscoreToCamel(name: String): String = "_([a-z\\d])".r.replaceAllIn(name, {m =>
      m.group(1).toUpperCase()
    })


    def makeFile(name: String, builder: TypeSpec.Builder): File = {
      File(s"${name}.java", Some(modelsDirectoryPath), JavaFile.builder(modelsNameSpace, builder.build).build.toString)
    }

    //TODO: we can use primitives as well, but then equal method needs to become smarter, this way is ok

    val dataTypes = Map[String, TypeName](
      "boolean" -> ClassName.get("java.lang", "Boolean"),
      "date-iso8601" -> ClassName.get("org.joda.time", "DateTime"),
      "date-time-iso8601" -> ClassName.get("org.joda.time", "DateTime"),
      "decimal" -> ClassName.get("java.math","BigDecimal"),
      "double" -> ClassName.get("java.lang","Double"),
      "integer" -> ClassName.get("java.lang", "Integer"),
      "long" -> ClassName.get("java.lang", "Long"),
      "object" -> ClassName.get("java.util","Map"),
      "string" -> ClassName.get("java.lang","String"),
      "unit" -> ClassName.get("java.lang", "Void"),
      "uuid" -> ClassName.get("java.util","UUID"),
      "map[string]" -> ClassName.get("java.util","Map")
    )

    def dataTypeFromField(`type`: String): TypeName = {
      dataTypes.get(`type`).getOrElse{
        val name = toParamName(`type`, false)
        if(isParameterArray(`type`))
          ArrayTypeName.of(ClassName.get(modelsNameSpace, name))
        else
          ClassName.get(modelsNameSpace, name)
      }
    }

  }
}
