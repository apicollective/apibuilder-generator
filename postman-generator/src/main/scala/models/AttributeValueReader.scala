package models

import io.postman.generator.attributes.v0.models.AttributeName
import _root_.play.api.libs.json.{JsError, JsSuccess, JsValue, Reads}
import _root_.play.api.Logging
import io.apibuilder.spec.v0.models.Attribute

import scala.reflect.{ClassTag, classTag}

object AttributeValueReader extends Logging {

  def findAndReadFirst[A : Reads : ClassTag](attributes: Seq[Attribute], attributeName: AttributeName): Option[A] = {
    attributes.collectFirst {
      case attr if attr.name.equalsIgnoreCase(attributeName.toString) =>
        tryRead[A](attributeName, attr.value)
    }.flatten
  }

  def tryRead[A : Reads : ClassTag](attributeName: AttributeName, json: JsValue): Option[A] = {
    val reads = implicitly[Reads[A]]
    reads.reads(json) match {
      case JsSuccess(entity, _) =>
        Some(entity)
      case JsError(errors) =>
        logger.warn(s"Attribute [$attributeName] value $json could not be read as ${classTag[A].runtimeClass.getName} / Errors: $errors")
        None
    }
  }

}
