package models.generator.kotlin

import org.scalatest.{FlatSpec, Matchers}

class KotlinUtilTest
  extends FlatSpec
    with Matchers
    with KotlinUtil {

  "toParameterName" should "convert array" in {
    toParamName("[some_param]", false) should be("SomeParam")
    toParamName("[some_param]", true) should be("someParam")
  }

  it should "convert object" in {
    toParamName("some_param", false) should be("SomeParam")
    toParamName("some_param", true) should be("someParam")

    toParamName("String", false) should be("String")
    toParamName("String", true) should be("string")
  }

  "makeNameSpace" should "respect reserved words" in {
    makeNameSpace("com.gilt.public.api") should be("com.gilt.public_.api")
    makeNameSpace("com.gilt.other.api") should be("com.gilt.other.api")
  }

  "isParameterArray" should "detect arrays" in {
    isParameterArray("[abc]") should be(true)
    isParameterArray("abc") should be(false)
    isParameterArray("[string]") should be(true)
    isParameterArray("string") should be(false)
    isParameterArray("not[string]") should be(false)
    isParameterArray("[string]not") should be(false)
  }

  "getArrayType" should "return array type" in {
    getArrayType("[abc]") should be("abc")
    getArrayType("[long]") should be("long")
    getArrayType("not[long]") should be("not[long]")
    getArrayType("not[long]") should be("not[long]")
    getArrayType("[long]not") should be("[long]not")
  }

  "isParameterMap" should "detect maps" in {
    isParameterMap("map[abc]") should be(true)
    isParameterMap("map[string]") should be(true)
    isParameterMap("notmap[abc]") should be(false)
    isParameterMap("mapnot[abc]") should be(false)
    isParameterMap("[abc]map") should be(false)
    isParameterMap("[abc]notmap") should be(false)
  }

  "getMapType" should "return map type" in {
    getMapType("map[abc]") should be("abc")
    getMapType("map[long]") should be("long")
    getMapType("notmap[long]") should be("notmap[long]")
    getMapType("map[long]not") should be("map[long]not")
  }

  "dataTypeFromField" should "produce simple types" in {
    dataTypeFromField("boolean", "com.foobar.example").toString should be ("kotlin.Boolean")
    dataTypeFromField("long", "com.foobar.example").toString should be ("kotlin.Long")
    dataTypeFromField("uuid", "com.foobar.example").toString should be ("java.util.UUID")
    dataTypeFromField("date-iso8601", "com.foobar.example").toString should be ("org.threeten.bp.LocalDate")
    dataTypeFromField("date-time-iso8601", "com.foobar.example").toString should be ("org.threeten.bp.Instant")
  }

  "isModelNameWithPackage" should "return correctly" in {
    isModelNameWithPackage("abc") should be(false)
    isModelNameWithPackage("io.apibuilder.common.v0.models.reference") should be(true)
  }

  "capitalizeModelNameWithPackage" should "capitalize last word" in {
    capitalizeModelNameWithPackage("io.apibuilder.common.v0.models.reference") should be("io.apibuilder.common.v0.models.Reference")
  }

  it should "handle arrays" in {
    dataTypeFromField("[long]", "com.foobar.example").toString should be ("kotlin.collections.List<kotlin.Long>")
    dataTypeFromField("[string]", "com.foobar.example").toString should be ("kotlin.collections.List<kotlin.String>")
    dataTypeFromField("[CustomType]", "com.foobar.example").toString should be ("kotlin.collections.List<com.foobar.example.CustomType>")
  }

  it should "handle maps" in {
    dataTypeFromField("map[long]", "com.foobar.example").toString should be ("kotlin.collections.Map<kotlin.String, kotlin.Long>")
    dataTypeFromField("map[date-time-iso8601]", "com.foobar.example").toString should be ("kotlin.collections.Map<kotlin.String, org.threeten.bp.Instant>")
    dataTypeFromField("map[date-iso8601]", "com.foobar.example").toString should be ("kotlin.collections.Map<kotlin.String, org.threeten.bp.LocalDate>")
    dataTypeFromField("map[string]", "com.foobar.example").toString should be ("kotlin.collections.Map<kotlin.String, kotlin.String>")
    dataTypeFromField("map[CustomType]", "com.foobar.example").toString should be ("kotlin.collections.Map<kotlin.String, com.foobar.example.CustomType>")
  }

  "textToComment" should "accept String" in {
    textToComment("Hello world") shouldBe "/**\n * Hello world\n */"
  }

  "textToComment" should "accept Seq(String)" in {
    textToComment(Seq("1+2", "2+3")) shouldBe "/**\n * 1+2\n * 2+3\n */"
  }

  "toMethodName" should "behave correctly" in {
    toMethodName("a") shouldBe "a"
    toMethodName("getThing") shouldBe "getThing"
    toMethodName("GetThing") shouldBe "getThing"
    toMethodName("Get_Thing") shouldBe "getThing"
    toMethodName("Get-Thing") shouldBe "getThing"
    toMethodName("Get Thing") shouldBe "getThing"
    toMethodName("Get$Thing") shouldBe "getThing"
    toMethodName("GetThing_") shouldBe "getThing"
  }
}
