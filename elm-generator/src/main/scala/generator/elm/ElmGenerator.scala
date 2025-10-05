package generator.elm

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.*
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
        generateModels(args),
        generateUnions(args),
        generateResources(args)
      ).mapN { case (a,b,c,d,e) => Seq(a,b,c,d,e) }.map { contents =>
        Seq(File(
          name = s"Generated/" + pascalServiceName(service) + ".elm",
          contents = generate(
            service,
            args,
            contents*
          )
        ))
      }
    }
  }

  private def pascalServiceName(service: Service): String = {
    val parts = service.namespace.split("\\.").filterNot(NamespaceParser.isVersion).toList
    Names.pascalCase(parts.distinct.mkString("_"))
  }

  private def generate(service: Service, args: GenArgs, contents: String*): String = {
    Seq(
      s"module Generated.${pascalServiceName(service)} exposing (..)",
      args.imports.generateCode(), // must be generated after the contents
      args.functions.generateCode(),
      trimTrailing(contents.filterNot(_.isEmpty).mkString("\n\n"))
    ).mkString("\n\n")
  }

  private def trimTrailing(str: String): String = {
    str.split("\n").toSeq.map(_.stripTrailing).mkString("\n").stripTrailing
  }


  private[elm] def generateModels(args: GenArgs): ValidatedNec[String, String] = {
    val models = ElmModel(args)
    args.service.models.map(models.generate).sequence.map(_.mkString("\n\n"))
  }

  private[elm] def generateUnions(args: GenArgs): ValidatedNec[String, String] = {
    val unions = ElmUnion(args)
    args.service.unions.map(unions.generate).sequence.map(_.mkString("\n\n"))
  }

  private def generateEnums(args: GenArgs): String = {
    val enums = ElmEnum(args)
    args.service.enums.map(enums.generate).mkString("\n\n")
  }

  private[elm] def generateResources(args: GenArgs): ValidatedNec[String, String] = {
    val resources = ElmResource(args)
    args.service.resources.map(resources.generate).sequence.map(_.mkString("\n\n"))
  }
}

case class GenArgs(service: Service) {

  val imports: Imports = Imports()

  val datatypeResolver: DatatypeResolver = GeneratorUtil.datatypeResolver(service)

  val functions: ElmFunctions = ElmFunctions()
}

case class VariableIndex() {
  private var index = 0
  def next(): String = {
    index = index + 1
    current
  }
  def current: String = s"v$index"
  def isFirst: Boolean = index == 0
}