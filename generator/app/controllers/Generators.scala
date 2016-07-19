package controllers

import com.bryzek.apidoc.generator.v0.models.Generator
import com.bryzek.apidoc.generator.v0.models.json._
import play.api.mvc._
import play.api.libs.json._
import lib.generator.{CodeGenerator, CodeGenTarget}

class Generators extends Controller {

  def get(
    key: Option[String] = None,
    limit: Integer = 100,
    offset: Integer = 0
  ) = Action { request: Request[AnyContent] =>
    val generators = Generators.targets.
      filter(t => t.codeGenerator.isDefined && t.status != lib.generator.Status.Proposal).
      filter(t => key.isEmpty || key == Some(t.metaData.key)).
      map(t => t.metaData)

    Ok(Json.toJson(generators.drop(offset).take(limit)))
  }

  def getByKey(key: String) = Action { request: Request[AnyContent] =>
    Generators.findGenerator(key) match {
      case Some((target, _)) => Ok(Json.toJson(target.metaData))
      case _ => NotFound
    }
  }

}

object Generators {

  def findGenerator(key: String): Option[(CodeGenTarget, CodeGenerator)] = for {
    target <- targets.find(_.metaData.key == key)
    codeGenerator <- target.codeGenerator
  } yield(target -> codeGenerator)

  val targets = Seq(
      CodeGenTarget(
        metaData = Generator(
          key = "anorm_2_x_parsers",
          name = "Anorm 2.x parsers",
          description = Some("Generates anorm parsers. Depends on apidoc_0_x_libs generators. See https://www.playframework.com/documentation/2.4.x/ScalaAnorm"),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Alpha,
        codeGenerator = Some(scala.generator.anorm.ParserGenerator)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "ruby_client",
          name = "Ruby client",
          description = Some("A pure ruby library to consume api.json web services. The ruby client has minimal dependencies and does not require any additional gems."),
          language = Some("Ruby")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(ruby.models.RubyClientGenerator)
      ),
       CodeGenTarget(
        metaData = Generator(
          key = "ning_1_9_client",
          name = "Ning Async Http Client 1.9",
          description = Some("Ning Async Http v. 1.9.x Client - see https://sonatype.github.io/async-http-client"),
          language = Some("Java, Scala")
        ),
        status = lib.generator.Status.Alpha,
        codeGenerator = Some(scala.models.ning.Ning19ClientGenerator)
      ),
       CodeGenTarget(
        metaData = Generator(
          key = "ning_1_8_client",
          name = "Ning Async Http Client 1.8",
          description = Some("Ning Async Http v. 1.8.x Client - see https://sonatype.github.io/async-http-client"),
          language = Some("Java, Scala")
        ),
        status = lib.generator.Status.Alpha,
        codeGenerator = Some(scala.models.ning.Ning18ClientGenerator)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_2_client",
          name = "Play 2.2 client",
          description = Some("Play Framework 2.2 client based on <a href='http://www.playframework.com/documentation/2.2.x/ScalaWS''>WS API</a>. Note this client does NOT support HTTP PATCH. If you need PATCH, we recommend using the ning client instead, which uses play-json underneath so should require minimal new dependencies in Play."),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play22ClientGenerator)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_3_client",
          name = "Play 2.3 client",
          description = Some("Play Framework 2.3 client based on <a href='http://www.playframework.com/documentation/2.3.x/ScalaWS'>WS API</a>."),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play23ClientGenerator)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_4_client",
          name = "Play 2.4 client",
          description = Some("Play Framework 2.4 client based on <a href='http://www.playframework.com/documentation/2.4.x/ScalaWS'>WS API</a>. Primary change from 2.3.x is WSRequestHolder has been deprecated (replaced by WSRequest)."),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play24ClientGenerator)
      ),
    CodeGenTarget(
      metaData = Generator(
        key = "play_2_5_client",
        name = "Play 2.5 client",
        description = Some("Play Framework 2.5 client based on <a href='http://www.playframework.com/documentation/2.5.x/ScalaWS'>WS API</a>. Primary change from 2.3.x is WSRequestHolder has been deprecated (replaced by WSRequest)."),
        language = Some("Scala")
      ),
      status = lib.generator.Status.Beta,
      codeGenerator = Some(scala.models.Play25ClientGenerator)
    ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_x_json",
          name = "Play 2.x json",
          description = Some("Generate play 2.x case classes with json serialization based on <a href='http://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators'>Scala Json combinators</a>. No need to use this target if you are already using the Play Client target."),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play2Models)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_x_standalone_json",
          name = "Play 2.x standalone json",
          description = Some("Generate case class with json serialization based on play-json, but do NOT include any other features that depend on the play framework (like QueryStringBindable)"),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play2StandaloneModelsJson)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "play_2_x_routes",
          name = "Play 2.x routes",
          description = Some("""Generate a routes file for play 2.x framework. See <a href="/doc/playRoutesFile">Play Routes File</a>."""),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.models.Play2RouteGenerator)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "scala_models",
          name = "Scala models",
          description = Some("Generate scala models from the API description."),
          language = Some("Scala")
        ),
        status = lib.generator.Status.Beta,
        codeGenerator = Some(scala.generator.ScalaCaseClasses)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "java_models",
          name = "Java models",
          description = Some("Generate Java models from the API description."),
          language = Some("Java")
        ),
        status = lib.generator.Status.InDevelopment,
        codeGenerator = Some(models.generator.JavaClasses)
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "swagger_json",
          name = "Swagger JSON",
          description = Some("Generate a valid swagger 2.0 json description of a service."),
          language = None
        ),
        status = lib.generator.Status.Proposal,
        codeGenerator = None
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "angular",
          name = "AngularJS client",
          description = Some("Generate a simple to use wrapper to access a service from AngularJS"),
          language = Some("JavaScript")
        ),
        status = lib.generator.Status.InDevelopment,
        codeGenerator = None
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "javascript",
          name = "Javascript client",
          description = Some("Generate a simple to use wrapper to access a service from javascript."),
          language = Some("JavaScript")
        ),
        status = lib.generator.Status.Proposal,
        codeGenerator = None
      ),
      CodeGenTarget(
        metaData = Generator(
          key = "go_1_5_client",
          name = "go 1.5 client",
          description = Some("Client based on native go 1.5 net/http libraries."),
          language = Some("Go"),
          attributes = Seq("go_import_mappings")
        ),
        status = lib.generator.Status.Alpha,
        codeGenerator = Some(go.models.GoClientGenerator)
      ),
       CodeGenTarget(
        metaData = Generator(
          key = "play_2_5_mock_client",
          name = "Play 2.5 Mock Client",
          description = Some("Provides a mock client with non functional, but compiling stubs, that can serve as a baseline for testing"),
          language = Some("Java, Scala")
        ),
        status = lib.generator.Status.Alpha,
        codeGenerator = Some(scala.generator.mock.MockClientGenerator.Play25)
      )
  ).sortBy(_.metaData.key)
}
