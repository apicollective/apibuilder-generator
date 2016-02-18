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


  // TODO: Figure out where we get the data from for the paths to
  // remove the Domains map
  private[this] val Domains = Map(
    "io.flow" -> "github.com/flowcommerce/apidoc"
  )


  // Ex: io.flow.carrier.account.v0.unions.expandable_carrier_account
  private[this] val ApidocUrlPattern = """^(.+)\.v\d+\.\w+\.?([^\.]*)$""".r

  /**
    * @param namespace The organization's namespace
    * @param goImportBaseUrl If provided, the base URL to use for imports.
    *        Example: github.com/flowcommerce/apidoc
    *        See http://apidoc.me/attributes/go_import_base_url
    */
  def apply(value: String, namespace: String, goImportBaseUrl: Option[String]): ImportPath = {
    value match {
      case ApidocUrlPattern(pkg, app) => {
        val defaultImportUrl = pkg.split("\\.").mkString("/")

        goImportBaseUrl match {
          case None => {
            ImportPath(defaultImportUrl, defaultAlias(defaultImportUrl))
          }

          case Some(baseUrl) => {
            pkg.startsWith(s"${namespace}.") match {
              case false => {
                ImportPath(defaultImportUrl, defaultAlias(defaultImportUrl))
              }

              case true => {
                val p = pkg.replace(s"${namespace}.", "")  // Ex: carrier.account
                val url = baseUrl + "/" + p.split("\\.").mkString("/")
                val alias = Text.snakeToCamelCase(p)
                ImportPath(url, alias)
              }
            }
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
