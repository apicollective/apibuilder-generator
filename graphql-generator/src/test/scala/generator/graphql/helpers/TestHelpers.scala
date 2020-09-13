package generator.graphql.helpers

trait TestHelpers {
  def rightOrErrors[T](value: Either[_, T]): T = {
    value match {
      case Right(r) => r
      case Left(errors) => sys.error(s"Expected valid value but got: ${errors}")
    }
  }
}
