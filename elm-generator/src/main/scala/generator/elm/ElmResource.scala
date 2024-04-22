package generator.elm

import cats.implicits._
import cats.data.ValidatedNec
import io.apibuilder.spec.v0.models.{Operation, Resource}

case class ElmResource(args: GenArgs) {
  private[this] val elmJson = ElmJson(args.imports)
  private[this] val elmType = ElmType(args)

  def generate(resource: Resource): ValidatedNec[String, String] = {
    elmType.validate(resource.`type`).map { resourceType =>
      resource.operations.map { op =>
        generate(resourceType, op)
      }.sequence.map(_.mkString("\n\n"))

    }
  }

  def generate(resourceType: String, op: Operation): ValidatedNec[String, String] = {

  }

  private[this] def name(resourceType: String, op: Operation): String = {

    op.method.toString.toLowerCase() + Names.pascalCase(resource.`type`)

  }
}
