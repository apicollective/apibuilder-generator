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
  private[this] val ApidocUrlPattern = """^(.+)\.v\d+\.\w+\.?([^\.]*)$""".r

  // TODO: Figure out where we get the data from for the paths to
  // remove the Domains map
  private[this] val Domains = Map(
    "io.flow" -> "github.com/flowcommerce/apidoc"
  )

  def apply(value: String): ImportPath = {
    value match {
      case ApidocUrlPattern(pkg, app) => {

        Domains.keys.toSeq.sortBy(_.length).reverse.find { key =>
          pkg.startsWith(s"${key}.")
        } match {
          case None => {
            ImportPath(value, defaultAlias(value))
          }

          case Some(domain) => {
            val p = pkg.replace(s"${domain}.", "")  // Ex: carrier.account
            val url = Domains(domain) + "/" + p.split("\\.").mkString("/")
            val alias = Text.snakeToCamelCase(p)
            ImportPath(url, alias)
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
    name.split("/").last
  }

}
