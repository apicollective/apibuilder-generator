package generator

import io.apibuilder.generator.v0.models.File
import lib.{Text, VersionTag}

object ServiceFileNames {

  def toFile(
    namespace: String,
    organizationKey: String,
    applicationKey: String,
    version: String,
    suffix: String,
    contents: String,
    languages: Option[String] = None
  ): File = {
    val language = languages.map { toLanguages(_) }.getOrElse(Nil).headOption.getOrElse(Language.Default)

    val baseName = Seq(
      Some(organizationKey),
      Some(applicationKey),
      VersionTag(version).major.map { v => s"V$v" },
      Some(suffix)
    ).flatten.mkString("_")

    val name = language.isCamelCased match {
      case true => Text.underscoreAndDashToInitCap(baseName)
      case false => Text.splitIntoWords(baseName).map(_.toLowerCase).mkString("_")
    }

    File(
      name = s"$name.${language.extension}",
      dir = Some(namespace.split("\\.").mkString("/")),
      contents = contents
    )
  }

  private[generator] sealed trait Language {
    def isCamelCased: Boolean
    def name: String
    def extension: String
  }

  private[generator] object Language {

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
        override def name = "kotlin"
        override def extension = "kt"
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
      },

      new Language {
        override def isCamelCased = false
        override def name = "json"
        override def extension = "json"
      }
    )

    def apply(language: String): Option[Language] = {
      All.find { _.name == language.trim.toLowerCase }
    }

    val Default = Language("text").getOrElse {
      sys.error("Failed to find text language")
    }

  }

  private[generator] def toLanguages(languages: String): Seq[Language] = {
    languages.split(",").map(_.trim).flatMap(Language(_))
  }

}
