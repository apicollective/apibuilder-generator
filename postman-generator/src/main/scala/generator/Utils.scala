package generator

import io.flow.postman.v0.models.Description
import io.flow.postman.v0.{models => postman}

object Utils {

  object Description {
    def apply(s: String): postman.Description = {
      postman.Description(content = Some(s), `type` = None)
    }
  }

  object Variable {
    def apply(
      key: String,
      value: String,
      `type`: String,
      id: Option[String] = None,
      name: Option[String] = None,
      description: Option[Description] = None,
      system: Option[Boolean] = None,
      disabled: Option[Boolean] = None
    ): postman.Variable = {
      postman.Variable(
        id = id,
        key = Some(key),
        value = Some(value),
        `type` = Some(`type`),
        name = name,
        description = description,
        system = system,
        disabled = disabled
      )
    }
  }

}
