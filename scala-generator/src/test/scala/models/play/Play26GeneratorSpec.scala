package scala.models.play

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import org.scalatest.{FunSpec, Matchers}

class Play26GeneratorSpec extends FunSpec with Matchers {
  it("format should ignore non scala files") {
    val contents = ""
    val file = File("", contents = contents)
    Play26Generator.format(file) should be(Right(file))
  }
}
