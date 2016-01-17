package lib.generator

import com.bryzek.apidoc.spec.v0.models.Method
import org.scalatest.{ShouldMatchers, FunSpec}

class GeneratorUtilSpec extends FunSpec with ShouldMatchers {

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
    GeneratorUtil.urlToMethodName(Seq("/memberships"), Method.Get, "/memberships") should be("get")
    GeneratorUtil.urlToMethodName(Seq("/memberships"), Method.Post, "/memberships") should be("post")
    GeneratorUtil.urlToMethodName(Seq("/memberships"), Method.Get, "/memberships/:guid") should be("getByGuid")
    GeneratorUtil.urlToMethodName(Seq("/memberships"), Method.Post, "/memberships/:guid/accept") should be("postAcceptByGuid")

    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Get, "/membership_requests") should be("get")
    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Post, "/membership_requests") should be("post")
    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Get, "/membership_requests/:guid") should be("getByGuid")

    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Get, "/membership-requests") should be("get")
    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Post, "/membership-requests") should be("post")
    GeneratorUtil.urlToMethodName(Seq("/membership_requests"), Method.Get, "/membership-requests/:guid") should be("getByGuid")

    GeneratorUtil.urlToMethodName(Nil, Method.Get, "/:key") should be("getByKey")
  }

  it("urlToMethodName for a complete service w/ named parameters") {
    val all = Seq(
      "/:orgKey",
      "/:orgKey/:applicationKey",
      "/:orgKey/:applicationKey/move",
      "/:orgKey/:applicationKey/:version/:generatorKey",
      "/:orgKey/:applicationKey/:version"
    )

    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey") should be("get")
    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey/:applicationKey") should be("getByApplicationKey")
    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey/:applicationKey/:version") should be("getByApplicationKeyAndVersion")
    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey/:applicationKey/move") should be("getMoveByApplicationKey")
    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey/:applicationKey/move/:id") should be("getMoveByApplicationKeyAndId")
    GeneratorUtil.urlToMethodName(all, Method.Get, "/:orgKey/:applicationKey/:version/:generatorKey") should be(
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
