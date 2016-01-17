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
