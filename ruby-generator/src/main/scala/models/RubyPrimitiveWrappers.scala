package ruby.models

import scala.util.Try

import lib.Datatype.Primitive

import com.gilt.apidoc.spec.v0.models.{Field, Model, Service, Union}

private[models] object RubyPrimitiveWrapper {

  def className(union: Union, primitive: Primitive): String = {
    RubyUtil.toClassName(Seq(union.name, primitive.toString, "wrapper").mkString("_"))
  }

}

private[models] case class RubyPrimitiveWrapper(service: Service) {

  private val primitives: Seq[Primitive] = service.unions.flatMap(_.types).map(_.`type`).flatMap { name =>
    Primitive(name).toOption
  }.distinct.sortBy(_.toString)

  case class Wrapper(model: Model, union: Union)

  def wrappers(): Seq[Wrapper] = {
    primitives.flatMap { p =>
      service.unions.filter(_.types.map(_.`type`).contains(p.name)).map { union =>
        val name = RubyPrimitiveWrapper.className(union, p)
        val model = Model(
          name = name,
          plural = s"${name}s",
          description = Some(s"Wrapper class to support the union type[${union.name}] datatype[${p.name}]"),
          fields = Seq(
            Field(
              name = "value",
              `type` = p.name,
              required = true
            )
          )
        )
        new Wrapper(model, union)
      }
    }
  }

}
