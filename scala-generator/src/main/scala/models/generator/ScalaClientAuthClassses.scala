package scala.generator

object ScalaClientAuthClassses {

  def apply(): String = """
sealed trait Authorization
object Authorization {
  case class Basic(username: String, password: Option[String] = None) extends Authorization
}
""".trim

}
