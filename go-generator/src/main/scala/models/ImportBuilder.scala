package go.models

import scala.collection.mutable
import lib.Text
import Formatter._
import com.bryzek.apidoc.spec.v0.models.Import

/**
  * Keeps track of imports that we use to build a list of imports for
  * only the packages actually used in the client.
  */
private[models] case class ImportBuilder(imports: Seq[Import]) {

  println("IMPORTS:")
  imports.map { imp =>
    println(s" - ${imp.namespace}")
  }

  // Build a list of go imports as we use them so we only import
  // libraries we actually use
  private[this] var importPaths = mutable.ListBuffer[ImportPath]()

  /**
    * Ensures that this library is being imported, returning the alias
    * used to reference this package
    * 
    * @param name e.g. "os", "net/http", "io.flow.common.v0.models.change_type", etc.
    */
  def ensureImport(name: String): String = {
    val path = ImportPath(name)

    importPaths.find(_.url == path.url) match {
      case None => {
        val alias = uniqueAlias(path)
        importPaths += path.copy(alias = alias)
        alias
      }
      case Some(existing) => {
        existing.alias
      }
    }
  }

  /**
    * Returns the public name for the specified object. If this is an
    * imported object, adds it to the import list and returns a name
    * w/ the proper prefix from the import alias
    */
  def publicName(name: String): String = {
    val i = name.lastIndexOf(".")
    (i > 0) match {
      case false => {
        GoUtil.publicName(name)
      }
      case true => {
        val ns = name.substring(0, i)
        val className = name.substring(i+1)
        val alias = ensureImport(ns)
        alias + "." + GoUtil.publicName(className)
      }
    }
  }

  /**
    * Returns the public name for the specified object. If this is an
    * imported object, adds it to the import list and returns a name
    * w/ the proper prefix from the import alias
    */
  def privateName(name: String): String = {
    GoUtil.privateName(publicName(name))
  }

  private[this] def uniqueAlias(path: ImportPath, index: Int = 0): String = {
    val target = index match {
      case 0 => path.alias
      case 1 => {
        path.url.split("/").toList match {
          case host :: org :: app :: rest => {
            // Ex: Turn github.com/flowcommerce/common into
            // flowcommerceCommon alias
            GoUtil.privateName(s"${org}_${path.alias}")
          }
          case parts => {
            GoUtil.privateName(parts.mkString("_"))
          }
        }
      }
      case _ => s"${path.alias}$index"
    }

    importPaths.find(_.alias == target) match {
      case None => target
      case Some(_) => uniqueAlias(path, index + 1)
    }
  }

  def generate(): String = {
    importPaths.toList match {
      case Nil => ""
      case _ => Seq(
        "import (",
        importPaths.sorted.map { imp =>
          if (ImportPath.defaultAlias(imp.url) == imp.alias) {
            GoUtil.wrapInQuotes(imp.url)
          } else {
            s"${imp.alias} ${GoUtil.wrapInQuotes(imp.url)}"
          }
        }.mkString("\n").indent(1),
        ")"
      ).mkString("\n")
    }

  }

}
