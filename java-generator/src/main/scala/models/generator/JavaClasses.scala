package models.generator

import lib.generator.{GeneratorUtil, CodeGenerator}

import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.{Field, Service, Model, Enum}

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
  override def invoke(form: InvocationForm): Either[Seq[String], String] = invoke(form, addHeader = true)

  def invoke(form: InvocationForm, addHeader: Boolean = false): Either[Seq[String], String] = Right(generateCode(form, addHeader))

  private def generateCode(form: InvocationForm, addHeader: Boolean = true): String = {
    val header = addHeader match {
      case false => None
      case true => Some(new ApidocComments(form.service.version, form.userAgent).forClassFile)
    }

    new Generator(form.service, header).generateService()
  }

  class Generator(service: Service, header: Option[String]) {
    private val datatypeResolver = GeneratorUtil.datatypeResolver(service)

    private val safeNamespace = service.namespace.split("\\.").map { JavaUtil.checkForReservedWord }.mkString(".")

    private val packageDeclaration = s"package $safeNamespace.models;"

    def generateService() = {
      val generatedModels = service.models.map { generateModel }.mkString("\n\n\n")

      val generatedEnums = service.enums.map { generateEnum }.mkString("\n\n\n")

      generatedModels + "\n\n\n" + generatedEnums
    }

    private def generateModel(model: Model): String = {
      def generateClassMember(field: Field) = {
        val datatype = datatypeResolver.parse(field.`type`, field.required).getOrElse {
          sys.error(s"Unable to parse datatype ${field.`type`}")
        }

        val javaDatatype = JavaDatatype(safeNamespace, datatype)

        val defaultValue = field.default.fold("") { " = " + javaDatatype.valueFromString(_) }

        field.description.fold("") { d => JavaUtil.textToComment(d) + "\n" } +
          s"private ${javaDatatype.name} ${JavaUtil.checkForReservedWord(field.name)}$defaultValue;"
      }

      val classDeclaration = {
        import lib.Text._

        val className = JavaUtil.toClassName(model.name)

        val noArgsConstructor = s"public $className() {}"

        model.description.fold("") { d => JavaUtil.textToComment(d) + "\n" } +
          "public class " + className + " {\n" +
          (
            model.fields.map { generateClassMember }.mkString("\n\n") + "\n\n" +
              noArgsConstructor
          ).indent(4) + "\n" +
          "}"
      }

      header.fold(""){ _ + "\n" } +
        packageDeclaration + "\n\n" +
        classDeclaration
    }

    private def generateEnum(enum: Enum): String = {
      val enumDeclaration = {
        import lib.Text._

        enum.description.fold("") { d => JavaUtil.textToComment(d) + "\n" } +
          "public enum " + JavaUtil.toClassName(enum.name) + " {\n" +
            enum.values.map { _.name.toUpperCase }.mkString(", ").indent(4) + "\n" +
          "}"
      }

      header.fold(""){ _ + "\n" } +
        packageDeclaration + "\n\n" +
        enumDeclaration
    }
  }
}
