package utils

object ScalaFormatter {

  def format(code: String): Either[Throwable, String] =
    org.scalafmt.Scalafmt.format(code, org.scalafmt.config.ScalafmtConfig.default120)
        .toEither

}
