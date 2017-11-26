package lib.generator

import io.apibuilder.spec.v0.models.Method
import org.scalatest.{Matchers, FunSpec}

class GeneratorUtilSpec extends FunSpec with Matchers {

  it("formatComment") {
    GeneratorUtil.formatComment("test") should be("# test")
    GeneratorUtil.formatComment("test this") should be("# test this")

    val source = "Search all users. Results are always paginated. You must specify at least 1 parameter"
    val target = """
# Search all users. Results are always paginated. You must specify at least 1
# parameter
""".trim
    GeneratorUtil.formatComment(source) should be(target)
  }

  it("urlToMethodName") {
    GeneratorUtil.urlToMethodName(None, Seq("/memberships"), Method.Get, "/memberships") should be("get")
    GeneratorUtil.urlToMethodName(None, Seq("/memberships"), Method.Post, "/memberships") should be("post")
    GeneratorUtil.urlToMethodName(None, Seq("/memberships"), Method.Get, "/memberships/:guid") should be("getByGuid")
    GeneratorUtil.urlToMethodName(None, Seq("/memberships"), Method.Post, "/memberships/:guid/accept") should be("postAcceptByGuid")

    GeneratorUtil.urlToMethodName(Some("/memberships"), Nil, Method.Get, "/memberships") should be("get")
    GeneratorUtil.urlToMethodName(Some("/memberships"), Nil, Method.Post, "/memberships") should be("post")
    GeneratorUtil.urlToMethodName(Some("/memberships"), Nil, Method.Get, "/memberships/:guid") should be("getByGuid")
    GeneratorUtil.urlToMethodName(Some("/memberships"), Nil, Method.Post, "/memberships/:guid/accept") should be("postAcceptByGuid")

    GeneratorUtil.urlToMethodName(None, Seq("/membership_requests"), Method.Get, "/membership_requests") should be("get")
    GeneratorUtil.urlToMethodName(None, Seq("/membership_requests"), Method.Post, "/membership_requests") should be("post")
    GeneratorUtil.urlToMethodName(None, Seq("/membership_requests"), Method.Get, "/membership_requests/:guid") should be("getByGuid")

    GeneratorUtil.urlToMethodName(Some("/membership_requests"), Nil, Method.Get, "/membership_requests") should be("get")
    GeneratorUtil.urlToMethodName(Some("/membership_requests"), Nil, Method.Post, "/membership_requests") should be("post")
    GeneratorUtil.urlToMethodName(Some("/membership_requests"), Nil, Method.Get, "/membership_requests/:guid") should be("getByGuid")

    GeneratorUtil.urlToMethodName(None, Nil, Method.Get, "/:key") should be("getByKey")
  }

  it("urlToMethodName for a complete service w/ named parameters") {
    val all = Seq(
      "/:orgKey",
      "/:orgKey/:applicationKey",
      "/:orgKey/:applicationKey/move",
      "/:orgKey/:applicationKey/:version/:generatorKey",
      "/:orgKey/:applicationKey/:version"
    )


    GeneratorUtil.urlToMethodName(None, all, Method.Get, "/:orgKey/:applicationKey") should be("getByApplicationKey")
    GeneratorUtil.urlToMethodName(None, all, Method.Get, "/:orgKey/:applicationKey/:version") should be("getByApplicationKeyAndVersion")
    GeneratorUtil.urlToMethodName(None, all, Method.Get, "/:orgKey/:applicationKey/move") should be("getMoveByApplicationKey")
    GeneratorUtil.urlToMethodName(None, all, Method.Get, "/:orgKey/:applicationKey/move/:id") should be("getMoveByApplicationKeyAndId")
    GeneratorUtil.urlToMethodName(None, all, Method.Get, "/:orgKey/:applicationKey/:version/:generatorKey") should be(
      "getByApplicationKeyAndVersionAndGeneratorKey"
    )

    GeneratorUtil.urlToMethodName(Some("/:orgKey"), Nil, Method.Get, "/:orgKey") should be("get")
    GeneratorUtil.urlToMethodName(Some("/:orgKey"), Nil, Method.Get, "/:orgKey/:applicationKey/:version") should be("getByApplicationKeyAndVersion")
    GeneratorUtil.urlToMethodName(Some("/:orgKey"), Nil, Method.Get, "/:orgKey/:applicationKey/move") should be("getMoveByApplicationKey")
    GeneratorUtil.urlToMethodName(Some("/:orgKey"), Nil, Method.Get, "/:orgKey/:applicationKey/move/:id") should be("getMoveByApplicationKeyAndId")
    GeneratorUtil.urlToMethodName(Some("/:orgKey"), Nil, Method.Get, "/:orgKey/:applicationKey/:version/:generatorKey") should be(
      "getByApplicationKeyAndVersionAndGeneratorKey"
    )
  }

  it("findLongestCommonPrefix") {
    GeneratorUtil.findLongestCommonPrefix(Seq("/foo", "/bar")) should be(Some("/"))
    GeneratorUtil.findLongestCommonPrefix(Seq("/users/foo", "/users/bar")) should be(Some("/users/"))
    GeneratorUtil.findLongestCommonPrefix(Seq("foo", "bar")) should be(None)
    GeneratorUtil.findLongestCommonPrefix(
      Seq(
        "/organizations/:key/groups/:group/members",
        "/organizations/:key/users/:id"
      )
    ) should be(Some("/organizations/:key/"))
  }

}
