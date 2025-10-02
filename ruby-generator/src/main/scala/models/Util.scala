package ruby.models

import io.apibuilder.spec.v0.models.*

object Util {

  def responseCodeAsString(code: ResponseCode): String = {
    code match {
      case ResponseCodeInt(value) => value.toString
      case ResponseCodeOption.Default => ResponseCodeOption.Default.toString
      case ResponseCodeOption.UNDEFINED(value) => value
      case ResponseCodeUndefinedType(value) => value
    }
  }

}

