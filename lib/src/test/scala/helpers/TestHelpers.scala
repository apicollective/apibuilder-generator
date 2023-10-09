package helpers

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec

trait TestHelpers {

  def rightOrErrors[T](value: Either[_, T]): T = {
    value match {
      case Right(r) => r
      case Left(errors) => sys.error(s"Expected valid value but got: $errors")
    }
  }

  def leftOrErrors[T](value: Either[T, _]): T = {
    value match {
      case Right(_) => sys.error("Expected a left value")
      case Left(r) => r
    }
  }
  def expectValid[T](r: ValidatedNec[String, T]): T = {
    r match {
      case Invalid(e) => sys.error(s"Expected valid but got: ${e.toNonEmptyList.toList}")
      case Valid(r) => r
    }
  }

}
