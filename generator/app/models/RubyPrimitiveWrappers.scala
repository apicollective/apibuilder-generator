package models

import lib.Primitives
import com.gilt.apidoc.spec.v0.models.{Field, Model, Service, Union}

private[models] object RubyPrimitiveWrapper {

  def className(union: Union, primitive: Primitives): String = {
    RubyUtil.toClassName(Seq(union.name, primitive.toString, "wrapper").mkString("_"))
  }

}

private[models] case class RubyPrimitiveWrapper(service: Service) {

  private val primitives = service.unions.flatMap(_.types).map(_.`type`).flatMap(Primitives(_)).distinct.sortBy(_.toString)

  case class Wrapper(model: Model, union: Union)

  def wrappers(): Seq[Wrapper] = {
    primitives.flatMap { p =>
      service.unions.filter(_.types.map(_.`type`).contains(p.toString)).map { union =>
        val name = RubyPrimitiveWrapper.className(union, p)
        val model = Model(
          name = name,
          plural = s"${name}s",
          description = Some(s"Wrapper class to support the union type[${union.name}] datatype[${p.toString}]"),
          fields = Seq(
            Field(
              name = "value",
              `type` = p.toString,
              required = true
            )
          )
        )
        new Wrapper(model, union)
      }
    }
  }

}
