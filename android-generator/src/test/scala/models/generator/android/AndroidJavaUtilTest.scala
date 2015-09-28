package models.generator.android

import org.scalatest.{Matchers, FlatSpec}
import AndroidJavaUtil._

class AndroidJavaUtilTest
  extends FlatSpec
  with Matchers{

  "toParameterName" should "convert array" in {
    toParamName("[some_param]") should be("SomeParam")
  }

  it should "convert object" in {
    toParamName("some_param") should be("SomeParam")
    toParamName("String") should be("String")
    toParamName("this.and.that") should be("ThisAndThat")
  }

}
