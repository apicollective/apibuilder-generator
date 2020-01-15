package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
import models.TestHelper._
import org.scalatest.{FunSpec, Matchers}
import scala.util.matching.Regex

class Play2ControllersSpec extends FunSpec with Matchers {

  def fileContent(service: Service): File =
    Play2Controllers.invoke(InvocationForm(service))
      .fold(
        { msgs => Left(new Throwable(s"Generated errors: ${msgs.mkString("\n  - ", "\n  - ", "")}")) },
        {
          case one :: Nil => Right(one)
          case _ :: _ => Left(new Throwable(s"Generated too many files"))
          case Nil => Left(new Throwable(s"Generated no files"))
        }
      )
      .fold(throwable => throw throwable, identity)

  def count(regex: Regex, service: Service): Int = {
    val contents = fileContent(service).contents
    regex.findAllIn(contents).length
  }

  def importCount(service: Service): Int = count("import ".r, service)
  def responseCount(service: Service): Int = count("trait [^ ]+Responses".r, service)
  def responseImplementationCount(service: Service): Int = count("case (class|object) HTTP[0-9]+".r, service)
  def serviceCount(service: Service): Int = count("trait [^ ]+Service".r, service)
  def serviceMethodCount(service: Service): Int = count("scala\\.concurrent\\.Future".r, service)
  def controllerCount(service: Service): Int = count("@javax\\.inject\\.Singleton".r, service)
  def controllerMethodCount(service: Service): Int = count("Action\\.async".r, service)

  describe("for all services") {
    List(
      collectionJsonDefaultsService,
      referenceApiService,
      referenceWithImportsApiService,
      generatorApiService,
      // apidocApiService,
      dateTimeService,
      builtInTypesService,
      generatorApiServiceWithUnionAndDescriminator,
      generatorApiServiceWithUnionWithoutDescriminator,
      emptyService,
    ).zipWithIndex.foreach { case (service, index) =>
      describe(s"for services ${index}") {
        it("generates all imports") {
          assert(importCount(service) == 1 + service.imports.size)
        }

        it("generates all responses") {
          assert(responseCount(service) == service.resources.flatMap(_.operations).size)
        }

        it("generates all response implementations") {
          assert(responseImplementationCount(service) == service.resources.flatMap(_.operations).flatMap(_.responses).size)
        }

        it("generates all services") {
          assert(serviceCount(service) == service.resources.size)
        }

        it("generates all service methods") {
          assert(serviceMethodCount(service) == service.resources.flatMap(_.operations).size)
        }

        it("generates all controllers") {
          assert(controllerCount(service) == service.resources.size)
        }

        it("generates all controller methods") {
          assert(controllerMethodCount(service) == service.resources.flatMap(_.operations).size)
        }
      }
    }
  }

}
