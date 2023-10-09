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
      (
        ElmCommon(args).generate().validNec,
        generateEnums(args).validNec,
        generateModels(args)
      ).mapN { case (a,b,c) => (a,b,c) }.map { case (common, enums, models) =>
        Seq(File(
          name = s"Generated/" + pascalServiceName(service) + ".elm",
          contents = generate(
            service,
            args.imports,
            common,
            enums,
            models
          )
        ))
      }
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

  private[elm] def generateModels(args: GenArgs): ValidatedNec[String, String] = {
    val models = ElmModel(args)
    args.service.models.map(models.generate).sequence.map(_.mkString("\n\n"))
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