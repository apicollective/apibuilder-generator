package lib

import com.bryzek.apidoc.generator.v0.models.File

object ServiceFileNames {

  def toFile(
    organizationKey: String,
    applicationKey: String,
    contents: String
  ): File = {
    val name = Seq(
      Text.underscoreAndDashToInitCap(organizationKey),
      Text.underscoreAndDashToInitCap(applicationKey),
      "ApidocClient.scala"
    ).mkString("")

    File(
      name = name,
      contents = contents
    )
  }

}
