package helpers

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

}
