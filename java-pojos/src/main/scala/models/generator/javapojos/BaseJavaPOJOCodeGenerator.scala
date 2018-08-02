package models.generator.javapojos

import java.io.IOException
import javax.lang.model.element.Modifier

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.core.{ Version}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.squareup.javapoet.AnnotationSpec.Builder
import com.squareup.javapoet._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Enum, Method, Model, ParameterLocation, Resource, ResponseCodeInt, Service, Union}
import lib.Text
import lib.generator.CodeGenerator
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, ISODateTimeFormat}

import scala.collection.JavaConverters._


trait BaseJavaPOJOCodeGenerator extends CodeGenerator with JavaPOJOUtil {

  def getJavaDocFileHeader() : String //abstract

  val JAVADOC_CLASS_MESSAGE = getJavaDocFileHeader()

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def invoke(form: InvocationForm, addHeader: Boolean = false): Either[Seq[String], Seq[File]] = Right(generateCode(form, addHeader))


  private def generateCode(form: InvocationForm, addHeader: Boolean = true): Seq[File] = {
    val header =
      if (addHeader) Some(new ApidocComments(form.service.version, form.userAgent).forClassFile)
      else None

    new Generator(form.service, header).generateSourceFiles()
  }


  class Generator(service: Service, header: Option[String]) {

    private val nameSpace = makeNameSpace(service.namespace)
    private val modelsNameSpace = nameSpace + ".models"
    private val modelsDirectoryPath = createDirectoryPath(modelsNameSpace)

    private val sharedObjectMapperClassName = "ApiBuilderObjectMapper"


    private val apiDocComments = {
      val s = JAVADOC_CLASS_MESSAGE + "\n"
      header.fold(s)(_ + "\n" + s)
    }

    val T = "$T" //this is a hack for substituting types, as "$" is used by scala to do string substitution, and $T is used by javapoet to handle types

    def createDirectoryPath(namespace: String) = namespace.replace('.', '/')

    def generateSourceFiles() = {

      val generatedEnums = service.enums.map {
        generateEnum
      }

      val generatedUnionTypes = service.unions.map {
        generateUnionType
      }

      val generatedModels = service.models.map { model =>
        val relatedUnions = service.unions.filter(_.types.exists(_.`type` == model.name))
        generateModel(model, relatedUnions)
      }

      val generatedObjectMapper = Seq(generateObjectMapper)

      val generatedResources = service.resources.map {
        generateResource
      }

      generatedEnums ++
        generatedUnionTypes ++
        generatedModels ++
        generatedObjectMapper ++
        generatedResources
    }


    def generateObjectMapper: File = {

      def formatter: FieldSpec.Builder = {
        FieldSpec.builder(classOf[DateTimeFormatter], "formatter", Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
          .initializer("$T.dateTimeParser()", classOf[ISODateTimeFormat])
      }


      val builder =
        TypeSpec.classBuilder(sharedObjectMapperClassName)
          .superclass(classOf[ObjectMapper])
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      builder.addField(formatter.build)

      val modifierField = FieldSpec.builder(classOf[ObjectMapper], "MAPPER").addModifiers(Modifier.PRIVATE).addModifiers(Modifier.FINAL).addModifiers(Modifier.STATIC)
      builder.addField(modifierField.build)

      builder.addStaticBlock(CodeBlock.builder
        .addStatement("SimpleModule module = new $T(new $T(1, 0, 0, null, null, null))", classOf[SimpleModule], classOf[Version])
        .addStatement("MAPPER = new ObjectMapper()")
        .addStatement("MAPPER.setPropertyNamingStrategy($T.CAMEL_CASE_TO_LOWER_CASE_WITH_UNDERSCORES)", classOf[PropertyNamingStrategy])
        .addStatement("MAPPER.configure($T.FAIL_ON_UNKNOWN_PROPERTIES, false)", classOf[DeserializationFeature])
        .addStatement("MAPPER.configure($T.READ_UNKNOWN_ENUM_VALUES_AS_NULL, true)", classOf[DeserializationFeature])
        .addStatement("MAPPER.registerModule(module)")
        .build)

      val getterBuilder = MethodSpec.methodBuilder("getInstance").addModifiers(Modifier.PUBLIC).addModifiers(Modifier.STATIC)
      getterBuilder.returns(classOf[ObjectMapper])
      getterBuilder.addStatement(s"return MAPPER")
      builder.addMethod(getterBuilder.build)

      File(s"${sharedObjectMapperClassName}.java", Some(nameSpace + "/javaPojos"), builder.build().toString)
    }

    def generateEnum(enum: Enum): File = {

      val className = toClassName(enum.name)

      val builder =
        TypeSpec.enumBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      enum.description.map(builder.addJavadoc(_))

      enum.values.foreach(value => {
        builder.addEnumConstant(toEnumName(value.name))
      })

      val nameFieldType = classOf[String]

      val constructorWithParams = MethodSpec.constructorBuilder()
      builder.addMethod(constructorWithParams.build())

      makeFile(className, builder)

    }

    def generateUnionType(union: Union): File = {
      val className = toClassName(union.name)

      val builder =
        TypeSpec.interfaceBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      union.description.map(builder.addJavadoc(_))
      makeFile(className, builder)
    }

    def generateModel(model: Model, relatedUnions: Seq[Union]): File = {


      val className = toClassName(model.name)

      val builder =
        TypeSpec.classBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      model.description.map(builder.addJavadoc(_))

      val constructorWithParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)

      val constructorWithoutParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)


      val unionClassTypeNames = relatedUnions.map { u => ClassName.get(modelsNameSpace, toClassName(u.name)) }
      builder.addSuperinterfaces(unionClassTypeNames.asJava)


      model.fields.foreach(field => {

        val fieldSnakeCaseName = field.name
        val arrayParameter = isParameterArray(field.`type`)
        val fieldCamelCaseName = toParamName(fieldSnakeCaseName, true)

        val javaDataType = dataTypeFromField(field.`type`, modelsNameSpace)

        val fieldBuilder = FieldSpec.builder(javaDataType, fieldCamelCaseName).addModifiers(Modifier.PRIVATE)
        builder.addField(fieldBuilder.build)

        val methodName = Text.snakeToCamelCase(s"get_${fieldSnakeCaseName}")

        val getterBuilder = MethodSpec.methodBuilder(methodName).addModifiers(Modifier.PUBLIC)
        getterBuilder.returns(javaDataType)
        getterBuilder.addStatement(s"return ${fieldCamelCaseName}")
        field.description.map(getterBuilder.addJavadoc(_))
        builder.addMethod(getterBuilder.build)

        val setterParameter = ParameterSpec.builder(javaDataType, fieldCamelCaseName)
        val setterBuilder = MethodSpec.methodBuilder(Text.snakeToCamelCase(s"set_${fieldSnakeCaseName}")).addModifiers(Modifier.PUBLIC)
        setterBuilder.addParameter(setterParameter.build)
        setterBuilder.addStatement("this.$N = $N", fieldCamelCaseName, fieldCamelCaseName)
        builder.addMethod(setterBuilder.build)

        val constructorParameter = ParameterSpec.builder(javaDataType, fieldCamelCaseName)
        constructorWithParams.addParameter(constructorParameter.build)
        constructorWithParams.addStatement("this.$N = $N", fieldCamelCaseName, fieldCamelCaseName)
      })

      builder.addMethod(constructorWithParams.build)
      builder.addMethod(constructorWithoutParams.build)

      makeFile(className, builder)

    }



    def generateResource(resource: Resource): File = {

      val className = toClassName(resource.plural) + "Client"

      val builder =
        TypeSpec.interfaceBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)


      makeFile(className, builder)
    }

    private def toMap(cc: AnyRef): Map[String, Any] =
      (Map[String, Any]() /: cc.getClass.getDeclaredFields) { (a, f) =>
        f.setAccessible(true)
        a + (f.getName -> f.get(cc))
      }


    private def commentFromOpt(opt: Option[String]) = {
      opt.fold("") { s => textToComment(s) + "\n" }
    }

    def camelToUnderscores(name: String): String = "[A-Z\\d]".r.replaceAllIn(name, { m =>
      "_" + m.group(0).toLowerCase()
    })


    def underscoreToCamel(name: String): String = "_([a-z\\d])".r.replaceAllIn(name, { m =>
      m.group(1).toUpperCase()
    })


    def makeFile(name: String, builder: TypeSpec.Builder): File = {
      File(s"${name}.java", Some(modelsDirectoryPath), JavaFile.builder(modelsNameSpace, builder.build).build.toString)
    }

  }

}
