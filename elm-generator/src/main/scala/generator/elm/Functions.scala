package generator.elm

import scala.collection.concurrent.TrieMap

case class Functions() {
  private[this] val all = TrieMap[String, Unit]()

  def add(body: String): Unit = {
    all.put(body.strip, ())
  }

  def generateCode(): String = {
    all.keysIterator.toSeq.sorted.mkString("\n\n")
  }
}


