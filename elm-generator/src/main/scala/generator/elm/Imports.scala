package generator.elm

import scala.collection.concurrent.TrieMap

case class Imports() {
  private[this] val allAs: TrieMap[String, String] = TrieMap[String, String]()
  private[this] val exposingAll: TrieMap[String, Unit] = TrieMap[String, Unit]()

  def addAs(name: String, as: String): Unit = {
    allAs.put(name, as).foreach { existing =>
      assert(existing == as, s"Import $name previously added as '$existing' - must match")
    }
    ()
  }

  def addExposingAll(name: String): Unit = {
    exposingAll.put(name, ())
    ()
  }

  def generateCode(): String = {
    (
      allAs.keysIterator.toSeq.sorted.map { name =>
       s"import $name as ${allAs(name)}"
      } ++ exposingAll.keysIterator.toSeq.sorted.map { name =>
        s"import $name exposing (..)"
      }
    ).mkString("\n")
  }
}


