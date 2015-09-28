package models.generator.android


import lib.Text
import lib.generator.CodeGenerator

import com.squareup.javapoet._
import javax.lang.model.element.Modifier
import com.bryzek.apidoc.spec.v0.models.Enum
import com.bryzek.apidoc.spec.v0.models.Union
import scala.Some
import com.bryzek.apidoc.generator.v0.models.File
import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.Model
import com.bryzek.apidoc.spec.v0.models.Resource
import com.bryzek.apidoc.spec.v0.models.Service
import com.fasterxml.jackson.annotation._
import com.bryzek.apidoc.spec.v0.models.Enum
import com.bryzek.apidoc.spec.v0.models.Union
import scala.Some
import com.bryzek.apidoc.generator.v0.models.File
import com.bryzek.apidoc.generator.v0.models.InvocationForm
import com.bryzek.apidoc.spec.v0.models.Model
import com.bryzek.apidoc.spec.v0.models.Resource
import com.bryzek.apidoc.spec.v0.models.Service


object AndroidClasses extends CodeGenerator {

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

    private val nameSpace = service.namespace.split("\\.").map { AndroidJavaUtil.checkForReservedWord }.mkString(".")
    private val modelsNameSpace = nameSpace + ".models"
    private val modelsDirectoryPath = createDirectoryPath(modelsNameSpace)

    def createDirectoryPath(namespace: String) = namespace.replace('.', '/')

    def generateSourceFiles() = {

      val generatedEnums = service.enums.map { generateEnum }

      val generatedModels = service.models.map { model =>
        generateModel(model, Seq.empty)
      }

      val generatedResources = service.resources.map { generateResource }

      generatedEnums ++
        generatedModels ++
        generatedResources


    }

    def generateEnum(enum: Enum): File = {

      val className = AndroidJavaUtil.toClassName(enum.name)

      val builder =
        TypeSpec.enumBuilder(className)
        .addModifiers(Modifier.PUBLIC)

      enum.description.map(builder.addJavadoc(_))

      enum.values.foreach(value => {
        builder.addEnumConstant(value.name)
      })

      makeFile(className, builder)

    }


    def generateModel(model: Model, relatedUnions: Seq[Union]): File = {


      val className = AndroidJavaUtil.toClassName(model.name)

      val builder =
        TypeSpec.classBuilder(className)
          .addModifiers(Modifier.PUBLIC)

      val jsonIgnorePropertiesAnnotation = AnnotationSpec.builder(classOf[JsonIgnoreProperties]).addMember("ignoreUnknown","true")
      builder.addAnnotation(jsonIgnorePropertiesAnnotation.build)

      model.description.map(builder.addJavadoc(_))

      val constructorWithParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)
      constructorWithParams.addAnnotation(classOf[JsonCreator])

      val equals = MethodSpec.methodBuilder("equals").addModifiers(Modifier.PUBLIC).addParameter(classOf[Object], "o").returns(classOf[Boolean]).addAnnotation(classOf[Override])
      equals.addCode("if (this == o) return true;\n")
      equals.addCode("if (o == null || getClass() != o.getClass()) return false;\n")
      equals.addCode(s"${className} that = (${className}) o;\n")

      val hashCode = MethodSpec.methodBuilder("hashCode").addModifiers(Modifier.PUBLIC).returns(classOf[Int]).addAnnotation(classOf[Override])
      hashCode.addCode("int result = 0;")

      model.fields.foreach(field => {

        val fieldSnakeCaseName = field.name
        val fieldCamelCaseName = AndroidJavaUtil.toParamName(fieldSnakeCaseName, true)

        val javaDataType: TypeName = dataTypes.get(field.`type`).getOrElse{
          val name = AndroidJavaUtil.toParamName(field.`type`, false)
          if(AndroidJavaUtil.isParameterArray(field.`type`))
            ArrayTypeName.of(ClassName.get(modelsNameSpace, name))
          else
            ClassName.get(modelsNameSpace, name)
        }

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

        equals.addCode(s"if (${fieldCamelCaseName} != null ? !${fieldCamelCaseName}.equals(that.${fieldCamelCaseName}) : that.${fieldCamelCaseName} != null) return false;\n")

        hashCode.addCode(s"result = 31 * result + (${fieldCamelCaseName} != null ? ${fieldCamelCaseName}.hashCode() : 0);\n")
      })

      equals.addCode("return true;\n")

      hashCode.addCode("return result;\n")

      builder.addMethod(constructorWithParams.build)
      builder.addMethod(equals.build)
      builder.addMethod(hashCode.build)

      makeFile(className, builder)

    }

    def generateResource(resource: Resource): File = {

      val className = AndroidJavaUtil.toClassName(resource.plural) + "Client"

      val builder =
        TypeSpec.interfaceBuilder(className)
          .addModifiers(Modifier.PUBLIC)

      makeFile(className, builder)

    }

    private def toMap(cc: AnyRef): Map[String,Any] =
      (Map[String, Any]() /: cc.getClass.getDeclaredFields) {(a, f) =>
        f.setAccessible(true)
        a + (f.getName -> f.get(cc))
      }


    private def commentFromOpt(opt: Option[String]) = {
      opt.fold("") { s => AndroidJavaUtil.textToComment(s) + "\n" }
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
      "date-iso8601" -> ClassName.get("java.util","Date"),
      "date-time-iso8601" -> ClassName.get("java.util","Date"),
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

  }
}
