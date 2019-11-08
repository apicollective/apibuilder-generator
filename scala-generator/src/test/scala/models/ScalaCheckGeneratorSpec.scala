package scala.models

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.Service
import models.TestHelper._
import org.scalatest.{FunSpec, Matchers}
import scala.util.matching.Regex

class ScalaCheckGeneratorSpec extends FunSpec with Matchers {

  def fileContent(service: Service): File =
    ScalaCheckGenerator.invoke(InvocationForm(service))
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

  def arbitraryCount(service: Service): Int = count("def arbitrary".r, service)
  def genCount(service: Service): Int = count("def gen".r, service)
  def importCount(service: Service): Int = count("import ".r, service)

  describe("for all services") {
    List(
      collectionJsonDefaultsService,
      referenceApiService,
      referenceWithImportsApiService,
      generatorApiService,
      apidocApiService,
      dateTimeService,
      builtInTypesService,
      generatorApiServiceWithUnionAndDescriminator,
      generatorApiServiceWithUnionWithoutDescriminator,
      emptyService,
    ).zipWithIndex.foreach { case (service, index) =>
      describe(s"for services ${index}") {
        val elementCount = service.enums.size +
          service.models.size +
          service.unions.size +
          1 // org.joda.time.DateTime

        it("generates all arbitraries") {
          assert(arbitraryCount(service) == elementCount)
        }

        it("generates all gens") {
          assert(genCount(service) == elementCount)
        }

        it("generates all imports") {
          assert(importCount(service) == service.imports.size + 1)
        }
      }
    }
  }

}
