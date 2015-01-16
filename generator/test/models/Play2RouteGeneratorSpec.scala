package models

import lib.{Datatype, Primitives, Type, Kind}
import com.gilt.apidoc.generator.v0.models.InvocationForm
import com.gilt.apidoc.spec.v0.models.{Method, Operation, Resource, Service}
import generator.{ScalaOperation, ScalaResource, ScalaService}
import org.scalatest.{ShouldMatchers, FunSpec}

class Play2RouteGeneratorSpec extends FunSpec with ShouldMatchers {

  def getScalaResource(ssd: ScalaService, name: String): ScalaResource = {
    ssd.resources.find(_.model.name == name).getOrElse {
      sys.error(s"Could not find $name resource. Available resources: " + ssd.resources.map(_.model.name).mkString(", "))
    }
  }

  def getScalaMethod(ssd: ScalaService, resourceName: String, method: Method, path: String): ScalaOperation = {
    val resource = getScalaResource(ssd, resourceName)
    resource.operations.filter { op => op.method == method && op.path == path }.headOption.getOrElse {
      val errorMsg = s"Operations found for $resourceName\n" + resource.operations.map { op =>
        "%s %s".format(op.method, op.path)
      }.mkString("\n")
      sys.error(s"Failed to find method[$method] with path[$path] for resource[${resourceName}]\n$errorMsg")
    }
  }

/*
  describe("with reference-api service") {
    lazy val service = TestHelper.referenceApiService
    lazy val ssd = new ScalaService(service)

    it("normalizes explicit paths that match resource name") {
      val resource = getScalaResource(ssd, "Organization")
      val op = getScalaMethod(ssd, "Organization", Method.Get, "/organizations")
      val r = Play2Route(ssd, op, resource)
      r.method should be("controllers.Organizations.get")
    }

    it("enums are strongly typed") {
      val resource = getScalaResource(ssd, "User")
      val op = getScalaMethod(ssd, "User", Method.Get, "/users/:age_group")
      val r = Play2Route(ssd, op, resource)
      r.method should be("controllers.Users.getByAgeGroup")
      r.params.mkString("") should be("age_group: com.gilt.apidoc.reference.api.models.AgeGroup")
    }

    it("supports multiple query parameters") {
      val echoResource = getScalaResource(ssd, "Echo")
      val op = getScalaMethod(ssd, "Echo", Method.Get, "/echoes")
      val r = Play2Route(ssd, op, echoResource)
      r.method should be("controllers.Echoes.get")
      r.params.mkString(" ") should be("foo: _root_.scala.Option[String]")
      r.paramComments.getOrElse("") should be("""
# Additional parameters to GET /echoes
#   - optional_messages: _root_.scala.Option[Seq[String]]
#   - required_messages: Seq[String]
""".trim)

      TestHelper.assertEqualsFile(
        "test/resources/generators/play-2-route-reference-api.routes",
        Play2RouteGenerator(InvocationForm(service)).invoke().getOrElse("")
      )
    }

    it("camel cases hypen in route") {
      val echoResource = getScalaResource(ssd, "Echo")
      val op = getScalaMethod(ssd, "Echo", Method.Get, "/echoes/arrays-only")
      val r = Play2Route(ssd, op, echoResource)
      r.method should be("controllers.Echoes.getArraysOnly")
    }

  }

  describe("with quality service example") {

    lazy val quality = ScalaService(TestHelper.parseFile("test/resources/examples/quality.json"))

    it("correctly orders parameters defined in path and parameters") {
      val op = getScalaMethod(quality, "Team", Method.Get, "/:org/teams/:key")
      op.parameters.map(_.name) should be(Seq("org", "key"))
      op.parameters.map(_.`type`.label) should be(Seq("string", "string"))
    }

  }
*/

  describe("with apidoc service") {
    lazy val service = TestHelper.apidocApiService
    lazy val ssd = new ScalaService(service)

    describe("users resource") {
      lazy val userResource = getScalaResource(ssd, "User")

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
      lazy val membershipRequestResource = getScalaResource(ssd, "MembershipRequest")

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
        val resource = getScalaResource(ssd, "Application")
        val op = getScalaMethod(ssd, "Application", Method.Get, "/:orgKey")
        val r = Play2Route(ssd, op, resource)
        r.method should be("controllers.Applications.getByOrgKey")
      }
    }
  }

}
