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
    private val modelsDirectoryPath = createDirectoryPath(nameSpace)

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

      model.description.map(builder.addJavadoc(_))

      val defaultConstructor = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)
      builder.addMethod(defaultConstructor.build)

      val constructorWithParams = MethodSpec.constructorBuilder().addModifiers(Modifier.PUBLIC)

      model.fields.foreach(field => {

        val javaDataType: TypeName = dataTypes.get(field.`type`).getOrElse{
          val name = AndroidJavaUtil.toParamName(field.`type`)
          if(AndroidJavaUtil.isParameterArray(field.`type`))
            ArrayTypeName.of(ClassName.get(nameSpace, name))
          else
            ClassName.get(nameSpace, name)
        }

        val fieldBuilder = FieldSpec.builder(javaDataType, field.name)
        fieldBuilder.addModifiers(Modifier.PRIVATE)
        builder.addField(fieldBuilder.build)

        val methodName = Text.snakeToCamelCase(s"get_${field.name}")

        val getterBuilder = MethodSpec.methodBuilder(methodName)
        getterBuilder.returns(javaDataType)
        getterBuilder.addStatement(s"return ${field.name}")
        field.description.map(getterBuilder.addJavadoc(_))
        builder.addMethod(getterBuilder.build)

        constructorWithParams.addParameter(javaDataType, field.name)
        constructorWithParams.addStatement("this.$N = $N", field.name, field.name)
      })

      builder.addMethod(constructorWithParams.build)

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
      File(s"${name}.java", Some(modelsDirectoryPath), JavaFile.builder(nameSpace, builder.build).build.toString)
    }


    val dataTypes = Map[String, TypeName](
      "boolean" -> TypeName.BOOLEAN,
      "date-iso8601" -> ClassName.get("java.util","Date"),
      "date-time-iso8601" -> ClassName.get("java.util","Date"),
      "decimal" -> ClassName.get("java.math","BigDecimal"),
      "double" -> ClassName.get("java.lang","Double"),
      "integer" -> TypeName.INT,
      "long" -> TypeName.LONG,
      "object" -> ClassName.get("java.util","Map"),
      "string" -> ClassName.get("java.lang","String"),
      "unit" -> TypeName.VOID,
      "uuid" -> ClassName.get("java.util","UUID"),
      "map[string]" -> ClassName.get("java.util","Map")
    )

  }
}
