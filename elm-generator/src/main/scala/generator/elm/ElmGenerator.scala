package generator.elm

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import lib.DatatypeResolver
import lib.generator.{CodeGenerator, GeneratorUtil}

object ElmGenerator extends CodeGenerator {

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    ElmGenerator().generate(form.service) match {
      case Invalid(errors) => Left(errors.toNonEmptyList.toList)
      case Valid(files) => Right(files)
    }
  }
}

case class ElmGenerator() {

  def generate(service: Service): ValidatedNec[String, Seq[File]] = {
    if (service.models.isEmpty) {
      "Service does not contain any models".invalidNec
    } else {
      val args = GenArgs(service)

      Seq(File(
        name = s"Generated/" + pascalServiceName(service) + ".elm",
        contents = generate(
          service,
          args.imports,
          ElmCommon(args).generate(),
          generateEnums(args),
          generateModels(args)
        )
      )).validNec
    }
  }

  private[this] def pascalServiceName(service: Service): String = {
    val parts = service.namespace.split("\\.").filterNot(NamespaceParser.isVersion).toList
    Names.pascalCase(parts.distinct.mkString("_"))
  }

  private[this] def generate(service: Service, imports: Imports, contents: String*): String = {
    Seq(
      s"module Generated.${pascalServiceName(service)} exposing (..)",
      imports.generateCode(), // must be generated after the content
      contents.mkString("\n\n")
    ).mkString("\n\n")
  }

  private[this] def generateModels(args: GenArgs): String = {
    val models = ElmModel(args)
    args.service.models.map(models.generate).mkString("\n\n")
  }

  private[this] def generateEnums(args: GenArgs): String = {
    val enums = ElmEnum(args)
    args.service.enums.map(enums.generate).mkString("\n\n")
  }
}

case class GenArgs(service: Service) {

  val imports: Imports = Imports()

  val datatypeResolver: DatatypeResolver = GeneratorUtil.datatypeResolver(service)

}