package models

import com.gilt.apidoc.spec.v0.models._

object Util {

  def responseCodeAsString(code: ResponseCode): String = {
    code match {
      case IntWrapper(value) => value.toString
      case ResponseCodeOption.Default => ResponseCodeOption.Default.toString
      case ResponseCodeOption.UNDEFINED(value) => value
      case ResponseCodeUndefinedType(value) => value
    }
  }

}

