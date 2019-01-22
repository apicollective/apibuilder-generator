package scala.models.play.files

import io.apibuilder.generator.v0.models.{File, InvocationForm}

object ModelsBodyParsers {

    val BadRequest = "_root_.play.api.mvc.Results.BadRequest"
    val BodyParser = "_root_.play.api.mvc.BodyParser"
    val ExecutionContext = "_root_.scala.concurrent.ExecutionContext"
    val JsError = "_root_.play.api.libs.json.JsError"
    val JsReads = "_root_.play.api.libs.json.Reads"
    val JsValue = "_root_.play.api.libs.json.JsValue"

    def bodyParser(): String = s"""
        private def bodyParser[A](parser: ${BodyParser}[${JsValue}])(implicit ec: ${ExecutionContext}, rds: ${JsReads}[A]): ${BodyParser}[A] =
            parser.validate(_.validate[A].asEither.left.map(e => ${BadRequest}(${JsError}.toJson(e))))
    """

    def bodyParser(enum: scala.generator.ScalaEnum): String = bodyParser(enum.name, enum.qualifiedName)
    def bodyParser(model: scala.generator.ScalaModel): String = bodyParser(model.name, model.qualifiedName)
    def bodyParser(union: scala.generator.ScalaUnion): String = bodyParser(union.name, union.qualifiedName)
    def bodyParser(suffix: String, tpe: String): String = s"""
        def bodyParser${suffix}(parser: ${BodyParser}[${JsValue}])(implicit ec: ${ExecutionContext}, rds: ${JsReads}[${tpe}]): ${BodyParser}[${tpe}] =
            bodyParser[${tpe}](parser)
    """

    def contents(form: InvocationForm): String = {
        val scalaService = scala.generator.ScalaService(form.service)

        val header = scala.models.ApidocComments(form.service.version, form.userAgent)

        val bodyParsers =
          scalaService.enums.map(bodyParser) ++
          scalaService.models.map(bodyParser) ++
          scalaService.unions.map(bodyParser)

        s"""
            ${header.toJavaString()}

            package ${scalaService.namespaces.models}

            package object bodyparsers {

              ${bodyParser}

              ${bodyParsers.mkString("\n")}

            }
        """
    }

    def file(form: InvocationForm, contents: String): File =
        generator.ServiceFileNames.toFile(
            form.service.namespace,
            form.service.organization.key,
            form.service.application.key,
            form.service.version,
            "ModelsBodyParsers",
            contents,
            Some("Scala")
        )

    def apply(form: InvocationForm): Either[Seq[String], File] = {
        val contents = this.contents(form)
        val file = this.file(form, contents)

        Right(file)
    }
}
