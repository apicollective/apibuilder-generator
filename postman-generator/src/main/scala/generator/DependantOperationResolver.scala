package generator

import io.apibuilder.spec.v0.models.{Field, Operation, Resource, Service}
import io.flow.postman.generator.attributes.v0.models.json._
import io.flow.postman.generator.attributes.v0.models.{AttributeName, ModelReference, PathReference}
import models.service.ResolvedService
import org.scalactic.TripleEquals._
import org.scalactic._
import models.attributes.PostmanAttributes._

object DependantOperationResolver {

  import scala.languageFeature.implicitConversions

  /**
    * 1. Searches for special postman attributes [[ExtendedObjectReference]] in whole Service and it's imports.
    * 2. Recursively resolves a dependant operations for each specified attributes.
    * 3. Returns a list of tuple from Attribute References [[ExtendedObjectReference]] and resolved Operation [[Operation]]
    *
    * For instance, 'buy_book_form' has a required 'book_id' param, that references to an existing book.
    * If a valid Apibuilder attribute is attached to 'book_id' param in the specification,
    * then this method will return a following tuple: (attribute, operation that creates a book)
    *
    * @param resolvedService - service with all imports at hand
    * @return a sequence of custom attribute to dependant operation pairs
    */
  def resolve(resolvedService: ResolvedService): Seq[(ExtendedObjectReference, Operation)] = {

    val service: Service = resolvedService.service
    val serviceNamespaceToResources: Map[String, Seq[Resource]] = resolvedService.serviceNamespaceToResources

    val attributesFromResources: Seq[PathReference] = {
      for {
        resource <- service.resources
        foundedPathAttr <- resource.attributes if foundedPathAttr.name.equalsIgnoreCase(AttributeName.ObjectReference.toString)
        pureAttr <- foundedPathAttr.value.asOpt[PathReference]
      } yield {
          pureAttr.copy(
            path = resource.path.flatMap(findParamNameInPathString)
          )
      }
    }

    val attributesFromModel = for {
      resource <- service.resources
      operation <- resource.operations
      body <- operation.body.toSeq
      attributes <- deepSearchModelsForAttributes(body.`type`, service)
    } yield attributes

    val allAttributes = attributesFromResources.map(_.toExtended) ++ attributesFromModel.map(_.toExtended)

    for {
      attribute <- allAttributes
      operation <- findOperationForAttribute(attribute, serviceNamespaceToResources).toSeq
    } yield {
      (attribute, operation)
    }
  }

  private def findReferenceAttributeInModelField(field: Field): Option[ModelReference] = {

    field.attributes.collectFirst {
      case attr if attr.name.equalsIgnoreCase(AttributeName.ObjectReference.toString) =>
        attr.value.asOpt[ModelReference]
    }.flatten
  }

  private def findOperationForAttribute(objRefAttr: ExtendedObjectReference, serviceNamespaceToResources: Map[String, Seq[Resource]]) = {
    serviceNamespaceToResources
      .getOrElse(key = objRefAttr.relatedServiceNamespace, default = Nil)
      .filter(_.`type` === objRefAttr.resourceType)
      .flatMap(_.operations)
      .find(_.method === objRefAttr.operationMethod)
  }

  private def deepSearchModelsForAttributes(typ: String, service: Service): Seq[ModelReference] = {

      def recurSearch(typ: String): Seq[ModelReference] = {
        service.models.find(_.name === typ) match {
          case Some(model) =>
            model.fields.flatMap {
              // model have fields with another models, going deeper
              case field if service.models.exists(_.name === field.`type`) =>
                recurSearch(field.`type`)
              // model is a "leaf", searching for special attribute
              case field =>
                val objRefAttrOpt = findReferenceAttributeInModelField(field)

                objRefAttrOpt match {
                  case Some(objAttrRef) =>
                    val modelToLookup =
                      if (objAttrRef.relatedServiceNamespace != service.namespace)
                        s"${objAttrRef.relatedServiceNamespace}.models.${objAttrRef.resourceType}"
                      else
                        objAttrRef.resourceType // namespace from the main service, plain model name
                    // adding and going recursive
                    recurSearch(modelToLookup) :+ objAttrRef
                  case None =>
                    Nil
                }
            }
          case None if service.unions.exists(_.name === typ) =>
            val union = service.unions.find(_.name === typ).get
            union.types.flatMap(u => recurSearch(u.`type`))
          case _ =>
            Nil
        }
      }

      recurSearch(typ)
  }

  private def findParamNameInPathString(path: String): Option[String] = {
    val regex = """\:(\w+)[\/]{0,1}""".r
    regex
      .findAllIn(path)
      .map(str => regex.replaceAllIn(str, "$1"))
      .toList
      .headOption //TODO investigate few params like ":organization/order/:id" in one resource path. Is it valid/used anywhere ?
  }

}
