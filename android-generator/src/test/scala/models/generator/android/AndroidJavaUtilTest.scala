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

}
