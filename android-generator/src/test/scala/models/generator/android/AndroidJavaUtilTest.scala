package models.generator.android

import org.scalatest.{Matchers, FlatSpec}

class AndroidJavaUtilTest
  extends FlatSpec
  with Matchers
  with AndroidJavaUtil{

  "toParameterName" should "convert array" in {
    toParamName("[some_param]", false) should be("SomeParam")
    toParamName("[some_param]", true) should be("someParam")
  }

  it should "convert object" in {
    toParamName("some_param", false) should be("SomeParam")
    toParamName("some_param", true) should be("someParam")

    toParamName("String", false) should be("String")
    toParamName("String", true) should be("string")

    toParamName("this.and.that", false) should be("ThisAndThat")
    toParamName("this.and.that", true) should be("thisAndThat")
  }

  "makeNameSpace" should "respect reserved words" in {
    makeNameSpace("com.gilt.public.api") should be("com.gilt.public_.api")
    makeNameSpace("com.gilt.other.api") should be("com.gilt.other.api")
  }

}
