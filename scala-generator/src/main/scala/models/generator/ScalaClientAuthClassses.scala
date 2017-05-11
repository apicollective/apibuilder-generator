package scala.generator

object ScalaClientAuthClassses {

  def apply(): String = """
sealed trait Authorization extends _root_.scala.Product with _root_.scala.Serializable
object Authorization {
  case class Basic(username: String, password: Option[String] = None) extends Authorization
}
""".trim

}
