package lib

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MethodsSpec extends AnyFunSpec with Matchers {

  it("isJsonDocumentMethod") {
    Methods.isJsonDocumentMethod("GET") should be(false)
    Methods.isJsonDocumentMethod("get") should be(false)
    Methods.isJsonDocumentMethod("DELETE") should be(false)
    Methods.isJsonDocumentMethod("delete") should be(false)
    Methods.isJsonDocumentMethod("POST") should be(true)
    Methods.isJsonDocumentMethod("post") should be(true)
    Methods.isJsonDocumentMethod("PUT") should be(true)
    Methods.isJsonDocumentMethod("put") should be(true)
    Methods.isJsonDocumentMethod("PATCH") should be(true)
    Methods.isJsonDocumentMethod("patch") should be(true)
  }

}
