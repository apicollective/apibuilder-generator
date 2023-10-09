package generator.elm

import scala.collection.concurrent.TrieMap

case class Imports() {
  private[this] val allAs: TrieMap[String, String] = TrieMap[String, String]()

  def addAs(name: String, as: String): Unit = {
    allAs.put(name, as)
    ()
  }

  def generateCode(): String = {
    allAs.keysIterator.toSeq.sorted.map { name =>
      s"import $name as ${allAs(name)}"
    }.mkString("\n")
  }
}


