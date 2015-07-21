package models.generator

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import com.bryzek.apidoc.spec.v0.models.{EnumValue, Union, Field, Service, Model, Enum}

import lib.generator.{GeneratorUtil, CodeGenerator}

import models.generator.JavaDatatypes.NativeDatatype

/**
 *
 * Author: jkenny
 * Date: 28/05/2015
 */
object JavaClasses extends CodeGenerator {
  /**
   * Invokes the code generators, returning either a list of errors
   * or the result of the code generation.
   */
  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = invoke(form, addHeader = true)

  def invoke(form: InvocationForm, addHeader: Boolean = false): Either[Seq[String], Seq[File]] = Right(generateCode(form, addHeader))

  private def generateCode(form: InvocationForm, addHeader: Boolean = true): Seq[File] = {
    val header =
      if (addHeader) Some(new ApidocComments(form.service.version, form.userAgent).forClassFile)
      else None

    new Generator(form.service, header).generateSourceFiles()
  }

  class Generator(service: Service, header: Option[String]) {
    private val datatypeResolver = GeneratorUtil.datatypeResolver(service)

    private val safeNamespace = service.namespace.split("\\.").map { JavaUtil.checkForReservedWord }.mkString(".")

    def createDirectoryPath(namespace: String) = namespace.replace('.', '/')

    // Keep everything (unions, enums, models, etc) in the same package to avoid headaches around importing
    private val modelsPackageDeclaration = s"package $safeNamespace.models;"
    private val modelsDirectoryPath = createDirectoryPath(s"$safeNamespace.models")

    def generateSourceFiles() = {

      val generatedEnums = service.enums.map { generateEnum }

      val generatedUnionTypes = service.unions.map { generateUnionType }

      val generatedUndefinedUnionTypes = service.unions.map { generateUndefinedUnionType }

      val generatedNativeWrappers = service.unions.flatMap { union =>
        union.types.collect {
          case native: NativeDatatype => generateNativeWrapper(union, native)
        }
      }

      val generatedModels = service.models.map { model =>
        val relatedUnions = service.unions.filter(_.types.exists(_.`type` == model.name))
        generateModel(model, relatedUnions)
      }

      generatedEnums ++
        generatedUnionTypes ++
        generatedUndefinedUnionTypes ++
        generatedNativeWrappers ++
        generatedModels
    }

    def generateEnum(enum: Enum): File = {
      def generateEnumValue(enumValue: EnumValue): String = {
        commentFromOpt(enumValue.description) +
          enumValue.name.toUpperCase
      }

      val className = JavaUtil.toClassName(enum.name)

      val enumDeclaration = {
        import lib.Text._

        commentFromOpt(enum.description) +
          s"public enum $className {\n" +
          enum.values.map { generateEnumValue }.mkString(",\n").indent(4) + "\n" +
          "}"
      }

      val source = header.fold(""){ _ + "\n" } +
        modelsPackageDeclaration + "\n\n" +
        enumDeclaration

      File(s"$className.java", Some(modelsDirectoryPath), source)
    }

    def generateUnionType(union: Union): File = {
      val className = JavaUtil.toClassName(union.name)
      val unionDeclaration = commentFromOpt(union.description) +
        s"public interface $className {}"

      val source = header.fold(""){ _ + "\n" } +
        modelsPackageDeclaration + "\n\n" +
        unionDeclaration

      File(s"$className.java", Some(modelsDirectoryPath), source)
    }

    def generateUndefinedUnionType(union: Union): File = {
      import lib.Datatype

      val className = JavaUtil.toClassName(union.name)
      val name = s"${className}UndefinedType"
      val undefinedUnionTypeModel = Model(
        name = name,
        plural = s"${name}s",
        description = Some(s"Provides future compatibility in clients - in the future, when a type is added to the union $className, it will need to be handled in the client code. This implementation will deserialize these future types as an instance of this class."),
        fields = Seq(
          Field(
            name = "description",
            description = Some(s"Information about the type that we received that is undefined in this version of the client."),
            `type` = Datatype.Primitive.String.name,
            required = true
          )
        )
      )

      generateModel(undefinedUnionTypeModel, Seq(union))
    }

    def generateNativeWrapper(union: Union, nativeDatatype: NativeDatatype): File = {
      val className = JavaUtil.toClassName(union.name)
      val datatypeClassName = JavaUtil.toClassName(nativeDatatype.shortName)
      val name = s"${className}${datatypeClassName}"
      val wrapperModel = Model(
        name = name,
        plural = s"${name}s",
        description = Some(s"Wrapper class to support the datatype '${nativeDatatype.apidocType}' in the union $className."),
        fields = Seq(
          Field(
            name = "value",
            `type` = nativeDatatype.apidocType,
            required = true
          )
        )
      )

      generateModel(wrapperModel, Seq(union))
    }

    def generateModel(model: Model, relatedUnions: Seq[Union]): File = {
      def generateClassMember(field: Field) = {
        val datatype = datatypeResolver.parse(field.`type`, field.required).getOrElse {
          sys.error(s"Unable to parse datatype ${field.`type`}")
        }

        val javaDatatype = JavaDatatype(datatype)

        val defaultValue = field.default.fold("") { " = " + javaDatatype.valueFromString(_) }

        commentFromOpt(field.description) +
          s"private ${javaDatatype.name} ${JavaUtil.checkForReservedWord(field.name)}$defaultValue;"
      }

      val className = JavaUtil.toClassName(model.name)

      val classDeclaration = {
        import lib.Text._

        val noArgsConstructor = s"public $className() {}"

        val unionClassNames = relatedUnions.map { u => JavaUtil.toClassName(u.name) }
        val implementsClause =
          if (unionClassNames.isEmpty) ""
          else unionClassNames.mkString(" implements ", ", ", "")

        commentFromOpt(model.description) +
          s"public class $className$implementsClause {\n" +
          (
            model.fields.map { generateClassMember }.mkString("\n\n") + "\n\n" +
              noArgsConstructor
          ).indent(4) + "\n" +
          "}"
      }

      val source = header.fold(""){ _ + "\n" } +
        modelsPackageDeclaration + "\n\n" +
        classDeclaration

      File(s"$className.java", Some(modelsDirectoryPath), source)
    }

    private def commentFromOpt(opt: Option[String]) = {
      opt.fold("") { s => JavaUtil.textToComment(s) + "\n" }
    }
  }
}
