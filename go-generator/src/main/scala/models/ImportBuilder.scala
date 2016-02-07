package go.models

import scala.collection.mutable
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

    // TODO: Check uniqueness
    println(s"$name => ($path, $alias)")

    imports.find(_.name == path) match {
      case None => imports += Import(name = path, alias = alias)
      case Some(_) => //No-op
    }

    alias
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
    val (unaliased, aliased) = imports.partition { imp => defaultAlias(imp.name) == imp.alias }

    Seq(
      (
        unaliased.toList match {
          case Nil => None
          case _ => Some(
            Seq(
              "import (",
              unaliased.map(_.name).sorted.map(GoUtil.wrapInQuotes(_)).mkString("\n").indent(1),
              ")"
            ).mkString("\n")
          )
        }
      ),

      (
        aliased.toList match {
          case Nil => None
          case _ => Some(
            aliased.sorted.map(imp => s"import ${imp.alias} " + GoUtil.wrapInQuotes(imp.name)).mkString("\n")
          )
        }
      )
    ).flatten.mkString("\n\n")
  }

}
