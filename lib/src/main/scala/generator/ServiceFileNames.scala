package generator

import com.bryzek.apidoc.generator.v0.models.File
import lib.Text

object ServiceFileNames {

  def toFile(
    namespace: String,
    organizationKey: String,
    applicationKey: String,
    contents: String,
    languages: Option[String] = None
  ): File = {
    val language = languages.map { toLanguages(_) }.getOrElse(Nil).headOption.getOrElse(Language.Default)

    val baseName = Seq(
      organizationKey,
      applicationKey,
      s"client"
    ).mkString("_")

    val name = language.isCamelCased match {
      case true => Text.underscoreAndDashToInitCap(baseName)
      case false => baseName
    }

    File(
      name = s"$name.${language.extension}",
      dir = Some(namespace.split("\\.").mkString("/")),
      contents = contents
    )
  }

  private[lib] sealed trait Language {
    def isCamelCased: Boolean
    def name: String
    def extension: String
  }

  private[lib] object Language {

    val All = Seq(
      new Language {
        override def isCamelCased = true
        override def name = "go"
        override def extension = "go"
      },

      new Language {
        override def isCamelCased = true
        override def name = "java"
        override def extension = "java"
      },

      new Language {
        override def isCamelCased = true
        override def name = "javascript"
        override def extension = "js"
      },

      new Language {
        override def isCamelCased = false
        override def name = "ruby"
        override def extension = "rb"
      },

      new Language {
        override def isCamelCased = true
        override def name = "scala"
        override def extension = "scala"
      },

      new Language {
        override def isCamelCased = true
        override def name = "swift"
        override def extension = "swift"
      },

      new Language {
        override def isCamelCased = false
        override def name = "text"
        override def extension = "txt"
      }
    )

    def apply(language: String): Option[Language] = {
      All.find { _.name == language.trim.toLowerCase }
    }

    val Default = Language("text").getOrElse {
      sys.error("Failed to find text language")
    }

  }

  private[lib] def toLanguages(languages: String): Seq[Language] = {
    languages.split(",").map(_.trim).flatMap(Language(_))
  }

}
