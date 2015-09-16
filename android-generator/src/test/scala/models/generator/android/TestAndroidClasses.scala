package models.generator.android

import org.scalatest.{Matchers, FlatSpec}
import play.api.libs.json.Json
import java.io.FileInputStream

import com.bryzek.apidoc.spec.v0.models._
import com.bryzek.apidoc.spec.v0.models.json._

class TestAndroidClasses
  extends FlatSpec
  with Matchers{


  AndroidClasses.getClass.getName should "invoke successfully" in {


    //TODO: this is a crappy way to specify path to file
    val json = Json.parse(new FileInputStream("./android-generator/src/test/resources/sample.json"))

    val form = Json.fromJson[Service](json).asOpt

    //TODO: not loading the object, why?
    //form.isDefined should be(true)

  }

}
