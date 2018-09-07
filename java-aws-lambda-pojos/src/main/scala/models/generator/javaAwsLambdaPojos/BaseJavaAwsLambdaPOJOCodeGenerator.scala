package models.generator.javaAwsLambdaPojos

import java.io.IOException

import javax.lang.model.element.Modifier
import com.squareup.javapoet.AnnotationSpec.Builder
import com.squareup.javapoet._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Enum, Method, Model, ParameterLocation, Resource, ResponseCodeInt, Service, Union}
import lib.Text
import lib.generator.CodeGenerator
import com.amazonaws.services.dynamodbv2.datamodeling._
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBTypeConvertedEnum
import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import me.geso.tinyvalidator.constraints._
import lombok.Data

import scala.collection.JavaConverters._


trait BaseJavaAwsLambdaPOJOCodeGenerator extends CodeGenerator with JavaAwsLambdaPOJOUtil {

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

      generatedEnums ++
        generatedUnionTypes ++
        generatedModels
    }

    def generateEnum(enum: Enum): File = {

      val className = toClassName(enum.name)

      val builder =
        TypeSpec.enumBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      enum.attributes.foreach(attribute => {
        attribute.name match {
          case "DynamoDBTypeConvertedEnum" =>{
            val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBTypeConvertedEnum])
            builder.addAnnotation(jsonAnnotationBuilder.build)
          }
          case _=> {}
        }

      })

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

    def isDynamoDbModel(model: Model): Boolean = {
      model.attributes.map(attribute => {
        attribute.name match {
          case "DynamoDBTable" => true
          case "DynamoDBDocument" => true
        }
      }).foldLeft(false)((x,y) => {x||y})
    }

    def generateModel(model: Model, relatedUnions: Seq[Union]): File = {

      val className = toClassName(model.name)

      val builder =
        TypeSpec.classBuilder(className)
          .addModifiers(Modifier.PUBLIC)
          .addJavadoc(apiDocComments)

      model.description.map(builder.addJavadoc(_))

      builder.addAnnotation(classOf[Data])
      val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[JsonIgnoreProperties])
      jsonAnnotationBuilder.addMember("ignoreUnknown",CodeBlock.builder().add("true").build)
      builder.addAnnotation(jsonAnnotationBuilder.build)

      model.attributes.foreach(attribute => {
        attribute.name match {
          case "DynamoDBTable" =>{
            val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBTable])
            jsonAnnotationBuilder.addMember("tableName","$S",(attribute.value \ "tableName").as[String])
            builder.addAnnotation(jsonAnnotationBuilder.build)
          }
          case "DynamoDBDocument"=>{
            val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBDocument])
            builder.addAnnotation(jsonAnnotationBuilder.build)
          }
          case _=> {}
        }

      })

      val constructorWithParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)

      val constructorWithoutParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)

      val unionClassTypeNames = relatedUnions.map { u => ClassName.get(modelsNameSpace, toClassName(u.name)) }
      builder.addSuperinterfaces(unionClassTypeNames.asJava)
      val shouldAnnotateAsDyanmoDb = isDynamoDbModel(model)

      model.fields.foreach(field => {

        val fieldSnakeCaseName = field.name
        val arrayParameter = isParameterArray(field.`type`)
        val fieldCamelCaseName = toParamName(fieldSnakeCaseName, true)

        val javaDataType = dataTypeFromField(field.`type`, modelsNameSpace)

        val fieldBuilder = FieldSpec.builder(javaDataType, fieldCamelCaseName).addModifiers(Modifier.PROTECTED)
        if(field.required){
          fieldBuilder.addAnnotation(classOf[NotNull])
        }

        val methodName = Text.snakeToCamelCase(s"get_${fieldSnakeCaseName}")

        val getterBuilder = MethodSpec.methodBuilder(methodName).addModifiers(Modifier.PUBLIC)

        field.attributes.foreach(attribute => {
          attribute.name match {
            case "DynamoDBRangeKey" =>{
              val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBRangeKey])
              jsonAnnotationBuilder.addMember("attributeName","$S",(attribute.value \ "attributeName").as[String])
              getterBuilder.addAnnotation(jsonAnnotationBuilder.build)
            }
            case "DynamoDBHashKey"=>{
              val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBHashKey])
              jsonAnnotationBuilder.addMember("attributeName","$S",(attribute.value \ "attributeName").as[String])
              getterBuilder.addAnnotation(jsonAnnotationBuilder.build)
            }
            case "size"=> {
              val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[Size])
              //What follows is some very strange behavior required to placate Scala's type system.
              jsonAnnotationBuilder.addMember("min","$L",new Integer((attribute.value \ "min").as[Int]))
              jsonAnnotationBuilder.addMember("max","$L",new Integer((attribute.value \ "max").as[Int]))
              fieldBuilder.addAnnotation(jsonAnnotationBuilder.build)
            }
            case "pattern"=> {
              val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[Pattern])
              jsonAnnotationBuilder.addMember("regexp","$S",(attribute.value \ "regexp").as[String])
              fieldBuilder.addAnnotation(jsonAnnotationBuilder.build)
            }
            case "httpUrl"=> {
              fieldBuilder.addAnnotation(classOf[HttpUrl])
            }
            case "email"=>{
              fieldBuilder.addAnnotation(classOf[Email])
            }
            case _ =>
          }
        })

        builder.addField(fieldBuilder.build)

        if(shouldAnnotateAsDyanmoDb && field.attributes.length == 0){
          val jsonAnnotationBuilder: Builder = AnnotationSpec.builder(classOf[DynamoDBAttribute])
          getterBuilder.addAnnotation(jsonAnnotationBuilder.build)
        }

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
