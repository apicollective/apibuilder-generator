package me.apidoc.lib.anorm

import com.bryzek.apidoc.generator.v0.models.{File, InvocationForm}
import org.scalatest.{ShouldMatchers, FunSpec}

class GeneratorSpec extends FunSpec with ShouldMatchers {

  it("generates lib") {
    val form = InvocationForm(models.TestHelper.apidocApiService)

    Generator.invoke(form) match {
      case Left(errors) => {
        fail(errors.mkString(", "))
      }
      case Right(files) => {
        files.map(_.name) should be(Seq("GiltApidocApiV0Anorm.scala"))
        models.TestHelper.assertEqualsFile("/apidoc/lib/anorm/util.txt", files.head.contents)
      }
    }
  }

}
