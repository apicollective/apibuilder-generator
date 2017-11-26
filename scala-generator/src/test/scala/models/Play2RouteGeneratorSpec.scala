package scala.models

import io.apibuilder.generator.v0.models.InvocationForm
import io.apibuilder.spec.v0.models.Method
import scala.generator.{ScalaOperation, ScalaResource, ScalaService}
import org.scalatest.{Matchers, FunSpec}

class Play2RouteGeneratorSpec extends FunSpec with Matchers {

  def getScalaResource(ssd: ScalaService, plural: String): ScalaResource = {
    ssd.resources.find(_.plural == plural).getOrElse {
      sys.error(s"Could not find $plural resource. Available resources: " + ssd.resources.map(_.plural).mkString(", "))
    }
  }

  def getScalaMethod(ssd: ScalaService, resourcePlural: String, method: Method, path: String): ScalaOperation = {
    val resource = getScalaResource(ssd, resourcePlural)
    resource.operations.filter { op => op.method == method && op.path == path }.headOption.getOrElse {
      val errorMsg = s"Operations found for $resourcePlural\n" + resource.operations.map { op =>
        "%s %s".format(op.method, op.path)
      }.mkString("\n")
      sys.error(s"Failed to find method[$method] with path[$path] for resource[$resourcePlural]\n$errorMsg")
    }
  }

  it("service with no operations") {
    val service = models.TestHelper.service(models.TestHelper.buildJson("""
      "imports": [],
      "headers": [],
      "info": [],
      "models": [],
      "enums": [],
      "unions": [],
      "models": [],
      "resources": [],
      "attributes": []
    """))

    Play2RouteGenerator(InvocationForm(service)).invoke() match {
      case Left(errors) => errors.mkString(", ") should be(
        s"Service[${service.organization.key}/${service.application.key}] does not have any resource operations - cannot generate play routes"
      )
      case Right(code) => fail("expected error when generating routes for a service with no operations")
    }
  }

  describe("with reference-api service") {
    lazy val service = models.TestHelper.referenceApiService
    lazy val ssd = new ScalaService(service)

    it("normalizes explicit paths that match resource name") {
      val resource = getScalaResource(ssd, "Organizations")
      val op = getScalaMethod(ssd, "Organizations", Method.Get, "/organizations")
      val r = Play2Route(ssd, op, resource)
      r.method should be("controllers.Organizations.get")
    }

    it("enums are strongly typed") {
      val resource = getScalaResource(ssd, "Users")
      val op = getScalaMethod(ssd, "Users", Method.Get, "/users/:age_group")
      val r = Play2Route(ssd, op, resource)
      r.method should be("controllers.Users.getByAgeGroup")
      r.params.mkString("") should be("age_group: io.apibuilder.reference.api.v0.models.AgeGroup")
    }

    it("supports multiple query parameters") {
      val echoResource = getScalaResource(ssd, "Echoes")
      val op = getScalaMethod(ssd, "Echoes", Method.Get, "/echoes")
      val r = Play2Route(ssd, op, echoResource)
      r.method should be("controllers.Echoes.get")
      r.params.mkString(", ") should be("foo: _root_.scala.Option[String], optional_messages: _root_.scala.Option[List[String]], required_messages: List[String]")

      Play2RouteGenerator(InvocationForm(service)).invoke() match {
        case Left(errors) => fail(errors.mkString(", "))
        case Right(sourceFiles) => {
          sourceFiles.size shouldBe 1
          models.TestHelper.assertEqualsFile(
            "/generators/play-2-route-reference-api.routes",
            sourceFiles.head.contents
         ) 
        }
      }
    }

    it("camel cases hypen in route") {
      val echoResource = getScalaResource(ssd, "Echoes")
      val op = getScalaMethod(ssd, "Echoes", Method.Get, "/echoes/arrays-only")
      val r = Play2Route(ssd, op, echoResource)
      r.method should be("controllers.Echoes.getArraysOnly")
    }

  }

  describe("with quality service example") {

    lazy val quality = ScalaService(models.TestHelper.parseFile("/examples/quality.json"))

    it("correctly orders parameters defined in path and parameters") {
      val op = getScalaMethod(quality, "Teams", Method.Get, "/:org/teams/:key")
      op.parameters.map(_.name) should be(Seq("org", "key"))
      op.parameters.map(_.`type`.name) should be(Seq("string", "string"))
    }

  }

  describe("with apidoc service") {
    lazy val service = models.TestHelper.apidocApiService
    lazy val ssd = new ScalaService(service)

    describe("users resource") {
      lazy val userResource = getScalaResource(ssd, "Users")

      it("GET w/ default path, parameters") {
        val op = userResource.operations.filter { op => op.method == Method.Get && op.path == "/users" }.head
        val r = Play2Route(ssd, op, userResource)
        r.verb should be(Method.Get)
        r.url should be("/users")
        r.method should be("controllers.Users.get")
        r.params.mkString(", ") should be("guid: _root_.scala.Option[_root_.java.util.UUID], email: _root_.scala.Option[String], token: _root_.scala.Option[String]")
      }

      it("GET w/ path, guid path param, no additional parameters") {
        val op = userResource.operations.filter { op => op.method == Method.Get && op.path == "/users/:guid" }.head
        val r = Play2Route(ssd, op, userResource)
        r.verb should be(Method.Get)
        r.url should be("/users/:guid")
        r.method should be("controllers.Users.getByGuid")
        r.params.mkString(", ") should be("guid: _root_.java.util.UUID")
      }

      it("POST w/ default path, no parameters") {
        val op = userResource.operations.filter { op => op.method == Method.Post && op.path == "/users" }.head
        val r = Play2Route(ssd, op, userResource)
        r.verb should be(Method.Post)
        r.url should be("/users")
        r.method should be("controllers.Users.post")
        r.params.mkString(", ") should be("")
      }

      it("PUT w/ guid in path, no parameters") {
        val op = userResource.operations.filter { op => op.method == Method.Put && op.path == "/users/:guid" }.head
        val r = Play2Route(ssd, op, userResource)
        r.verb should be(Method.Put)
        r.url should be("/users/:guid")
        r.method should be("controllers.Users.putByGuid")
        r.params.mkString(", ") should be("guid: _root_.java.util.UUID")
      }
    }

    describe("membership_request resource") {
      lazy val membershipRequestResource = getScalaResource(ssd, "MembershipRequests")

      it("POST /membership_requests/:guid/accept") {
        val op = membershipRequestResource.operations.filter { op => op.method == Method.Post && op.path == "/membership_requests/:guid/accept" }.head
        val r = Play2Route(ssd, op, membershipRequestResource)
        r.verb should be(Method.Post)
        r.url should be("/membership_requests/:guid/accept")
        r.method should be("controllers.MembershipRequests.postAcceptByGuid")
        r.params.mkString(", ") should be("guid: _root_.java.util.UUID")
      }
    }

    describe("application resource") {
      it("GET /:orgKey") {
        val resource = getScalaResource(ssd, "Applications")
        val op = getScalaMethod(ssd, "Applications", Method.Get, "/:orgKey")
        val r = Play2Route(ssd, op, resource)
        r.method should be("controllers.Applications.get")
      }
    }
  }

}
