package generator

case class Namespaces(base: String) {

  val models: String = s"$base.models"

  /**
    * We use a single namespace for models and enums to minimize
    * number of imports that users will need.
    */
  val enums: String = models
  val unions: String = models

  val errors: String = s"$base.errors"

  val last: String = base.split("\\.").last

}
