package go.models

import lib.Text

case class ImportPath(url: String, alias: String) extends Ordered[ImportPath] {

  def compare(other: ImportPath) = {
    url == other.url match {
      case false => url.compare(other.url)
      case true => alias.compare(other.alias)
    }
  }

}


object ImportPath {

  // Ex: io.flow.carrier.account.v0.unions.expandable_carrier_account
  private[this] val ApibuilderUrlPattern = """^(.+)\.v\d+\.\w+\.?([^\.]*)$""".r

  def apply(value: String, mappings: Map[String, String]): ImportPath = {
    value match {
      case ApibuilderUrlPattern(pkg, app) => {

        mappings.keys.toSeq.sortBy(_.length).reverse.find { key =>
          pkg.startsWith(s"${key}.")
        } match {
          case None => {
            val defaultUrl = pkg.split("\\.").mkString("/")
            ImportPath(defaultUrl, defaultAlias(defaultUrl))
          }

          case Some(domain) => {
            val p = pkg.replace(s"${domain}.", "")  // Ex: carrier.account
            val url = mappings(domain) + "/" + p.split("\\.").mkString("/")
            val alias = Text.snakeToCamelCase(p)
            ImportPath(url, GoUtil.quoteNameIfKeyword(alias))
          }
        }
      }

      case _ => {
        ImportPath(value, defaultAlias(value))
      }
    }
  }

  /**
    * This returns the default alias for the specified import as go
    * will interpret it. For example, if you import "net/http", the
    * default alias is 'http'.
    */
  def defaultAlias(name: String): String = {
    GoUtil.quoteNameIfKeyword(name.split("/").last)
  }

}
