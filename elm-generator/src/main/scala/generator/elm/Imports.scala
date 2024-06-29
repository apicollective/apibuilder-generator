package generator.elm

import scala.collection.concurrent.TrieMap

case class Imports() {
  private sealed trait ExposingAllValue
  private object ExposingAllValue {
    case object Wildcard extends ExposingAllValue
    case class Types(types: Seq[String]) extends  ExposingAllValue
  }

  private val allAs: TrieMap[String, String] = TrieMap[String, String]()
  private val exposingAll: TrieMap[String, ExposingAllValue] = TrieMap[String, ExposingAllValue]()

  def addAs(name: String, as: String): Unit = {
    allAs.put(name, as).foreach { existing =>
      assert(existing == as, s"Import $name previously added as '$existing' - must match")
    }
    ()
  }

  def addExposingAll(name: String): Unit = {
    exposingAll.put(name, ExposingAllValue.Wildcard)
    ()
  }

  def addExposing(name: String, types: String): Unit = addExposing(name, Seq(types))

  private def addExposing(name: String, types: Seq[String]): Unit = {
    exposingAll.get(name) match {
      case None => exposingAll.put(name, ExposingAllValue.Types(types))
      case Some(ExposingAllValue.Wildcard) => ()
      case Some(ExposingAllValue.Types(existing)) => exposingAll.put(name, ExposingAllValue.Types(existing ++ types))
    }
    ()
  }

  def generateCode(): String = {
    (
      allAs.keysIterator.toSeq.sorted.map { name =>
        val alias = allAs(name)
        if (alias == name) {
          s"import $name"
        } else {
          s"import $name as ${allAs(name)}"
        }
      } ++ exposingAll.keysIterator.toSeq.sorted.map { name =>
        exposingAll(name) match {
          case ExposingAllValue.Wildcard => s"import $name exposing (..)"
          case ExposingAllValue.Types(types) => s"import $name exposing (${types.distinct.sorted.mkString(", ")})"
        }
      }
    ).mkString("\n")
  }
}


