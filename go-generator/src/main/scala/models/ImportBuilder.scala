package go.models

import scala.collection.mutable
import lib.Text
import Formatter._

/**
  * Keeps track of imports that we use to build a list of imports for
  * only the packages actually used in the client.
  */
private[models] case class ImportBuilder() {

  private[this] case class Import(name: String, alias: String) extends Ordered[Import] {

    def compare(other: Import) = {
      name == other.name match {
        case false => name.compare(other.name)
        case true => alias.compare(other.alias)
      }
    }

  }

  // Build a list of go imports as we use them so we only import
  // libraries we actually use
  private[this] var imports = mutable.ListBuffer[Import]()

  /**
    * Ensures that this library is being imported, returning the alias
    * used to reference this package
    */
  def ensureImport(name: String): String = {
    val path = importPath(name)
    val alias = defaultAlias(name)

    imports.find(_.name == path) match {
      case None => {
        imports += Import(name = path, alias = uniqueAlias(path, alias))
      }
      case Some(_) => //No-op
    }

    alias
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

  private[this] def uniqueAlias(importPath: String, name: String, index: Int = 0): String = {
    val target = index match {
      case 0 => name
      case 1 => {
        importPath.split("/").toList match {
          case host :: org :: app :: rest => {
            // Ex: Turn github.com/flowcommerce/common into
            // flowcommerceCommon alias
            GoUtil.privateName(s"${org}_$name")
          }
          case parts => {
            Text.snakeToCamelCase(parts.mkString("_"))
          }
        }
      }
      case _ => s"$name$index"
    }

    imports.find(_.name == target) match {
      case None => target
      case Some(_) => uniqueAlias(importPath, name, index + 1)
    }
  }

  private[this] def defaultAlias(name: String): String = {
    name.split("\\.").toList match {
      case Nil => ""
      case one :: Nil => one.split("/").last
      case one :: two :: Nil => two
      case one :: two :: three :: rest => three
    }
  }

  // TODO: Figure out where we get the data from for the paths to
  // remove the 'commerce' hack
  private[this] def importPath(name: String): String = {
    name.split("\\.").toList match {
      case Nil => ""
      case one :: Nil => one
      case one :: two :: Nil => s"github.com/${one}commerce/$two"
      case one :: two :: three :: rest => s"github.com/${two}commerce/$three"
    }
  }

  def generate(): String = {
    imports.map { imp =>
    }

    imports.toList match {
      case Nil => ""
      case _ => Seq(
        "import (",
        imports.sorted.map { imp =>
          if (defaultAlias(imp.name) == imp.alias) {
            GoUtil.wrapInQuotes(imp.name)
          } else {
            s"${imp.alias} ${GoUtil.wrapInQuotes(imp.name)}"
          }
        }.mkString("\n").indent(1),
        ")"
      ).mkString("\n")
    }

  }

}
