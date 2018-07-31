package models.generator.android

import org.scalatest.{FlatSpec, Matchers}

class JavaPOJOUtilTest
  extends FlatSpec
  with Matchers
  with JavaPOJOUtil{

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
    getArrayType("[[abc]]") should be("[abc]")
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
    getMapType("map[map[abc]]") should be("map[abc]")
    getMapType("map[long]") should be("long")
    getMapType("notmap[long]") should be("notmap[long]")
    getMapType("map[long]not") should be("map[long]not")
    getMapType("map[map[[long]]]") should be("map[[long]]")
    getMapType("map[map[map[[long]]]]") should be("map[map[[long]]]")
  }

  "dataTypeFromField" should "produce simple types" in {
    dataTypeFromField("boolean", "com.apidoc.example").toString should be ("java.lang.Boolean")
    dataTypeFromField("long", "com.apidoc.example").toString should be ("java.lang.Long")
    dataTypeFromField("uuid", "com.apidoc.example").toString should be ("java.util.UUID")
    dataTypeFromField("date-iso8601", "com.apidoc.example").toString should be ("org.joda.time.DateTime")
    dataTypeFromField("date-time-iso8601", "com.apidoc.example").toString should be ("org.joda.time.DateTime")
  }

  "isModelNameWithPackage" should "return correctly" in {
    isModelNameWithPackage("abc") should be(false)
    isModelNameWithPackage("io.apibuilder.common.v0.models.reference") should be(true)
  }

  "capitalizeModelNameWithPackage" should "capitalize last word" in {
    capitalizeModelNameWithPackage("io.apibuilder.common.v0.models.reference") should be("io.apibuilder.common.v0.models.Reference")
  }

  it should "handle arrays" in {
    dataTypeFromField("[long]", "com.apidoc.example").toString should be ("java.lang.Long[]")
    dataTypeFromField("[string]", "com.apidoc.example").toString should be ("java.lang.String[]")
    dataTypeFromField("[CustomType]", "com.apidoc.example").toString should be ("com.apidoc.example.CustomType[]")
  }

  it should "handle maps" in {
    dataTypeFromField("map[long]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, java.lang.Long>")
    dataTypeFromField("map[date-time-iso8601]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, org.joda.time.DateTime>")
    dataTypeFromField("map[string]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, java.lang.String>")
    dataTypeFromField("map[CustomType]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, com.apidoc.example.CustomType>")
  }

  it should "handle map and array combinations" in {
    dataTypeFromField("map[map[CustomType]]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, java.util.Map<java.lang.String, com.apidoc.example.CustomType>>")
    dataTypeFromField("map[map[[CustomType]]]", "com.apidoc.example").toString should be ("java.util.Map<java.lang.String, java.util.Map<java.lang.String, com.apidoc.example.CustomType[]>>")
  }

  "replaceEnumsPrefixWithModels" should "replace enums prefix correctly" in {
    replaceEnumsPrefixWithModels("com.gilt.commons.enums.v0.enums.Store") should be("com.gilt.commons.enums.v0.models.Store")
    replaceEnumsPrefixWithModels("com.gilt.commons.enums.v0.models.Store") should be("com.gilt.commons.enums.v0.models.Store")
    replaceEnumsPrefixWithModels("com.gilt.commons.enums.v0.other.Store") should be("com.gilt.commons.enums.v0.other.Store")
    replaceEnumsPrefixWithModels("com.gilt.commons.enums.enums.enums.enums") should be("com.gilt.commons.enums.enums.models.enums")
    replaceEnumsPrefixWithModels("com.gilt.ClassName") should be("com.gilt.ClassName")
    replaceEnumsPrefixWithModels("com.enums.ClassName") should be("com.models.ClassName")
    replaceEnumsPrefixWithModels("com.ClassName") should be("com.ClassName")
    replaceEnumsPrefixWithModels("enums.ClassName") should be("models.ClassName")

  }

}
