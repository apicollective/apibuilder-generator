package go.models

import scala.collection.mutable

/**
  * Keeps track of imports that we use to build a list of imports for
  * only the packages actually used in the client.
  */
private[models] case class ImportBuilder() {

  // Build a list of go imports as we use them so we only import
  // libraries we actually use
  private[this] var imports = mutable.ListBuffer[String]()

  def ensureImport(name: String) {
    imports.find(_ == name) match {
      case None => imports += name
      case Some(_) => //No-op
    }
  }

  def generate(): String = {
    Seq(
      "import (",
      imports.sorted.map(GoUtil.wrapInQuotes(_)).mkString("\t", "\n\t", ""),
      ")"
    ).mkString("\n")
  }

}
