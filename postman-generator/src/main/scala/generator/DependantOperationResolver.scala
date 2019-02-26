package generator

import io.apibuilder.spec.v0.models.{Operation, Resource, Service}
import io.flow.postman.generator.attributes.v0.models.ObjectReference
import io.flow.postman.generator.attributes.v0.models.AttributeName
import io.flow.postman.generator.attributes.v0.models.json.jsonReadsPostmanGeneratorAttributesObjectReference
import models.service.ResolvedService

object DependantOperationResolver {

  /**
    * Resolves a list of the dependant operations according to the specified attributes.
    * For instance, 'buy_book_form' has a required 'book_id' param, that references to an existing book.
    * If a valid Apibuilder attribute is attached to 'book_id' param in the specification,
    * then this method will return a following tuple: (attribute, operation that creates a book)
    *
    * @param resolvedService - service with all imports at hand
    * @return a sequence of custom attribute to dependant operation pairs
    */
  def resolve(resolvedService: ResolvedService): Seq[(ObjectReference, Operation)] = {

    val service: Service = resolvedService.service
    val serviceNamespaceToResources: Map[String, Seq[Resource]] = resolvedService.serviceNamespaceToResources

    // TODO: think about the model that can have yet another id dependency in the endpoint that creates it - is it already covered?
    def recurOps(typ: String): Seq[ObjectReference] = {

      service.models.find(_.name == typ) match {
        case Some(model) =>
          model.fields.flatMap {
            case field if service.models.exists(_.name == field.`type`) =>
              recurOps(field.`type`)
            case field =>
              val objRefAttrOpt = field.attributes.collectFirst {
                case attr if attr.name.equalsIgnoreCase(AttributeName.ObjectReference.toString) =>
                  attr.value.asOpt[ObjectReference]
              }.flatten

              objRefAttrOpt match {
                case Some(objAttrRef) =>
                  val modelToLookup = if (objAttrRef.relatedServiceNamespace != service.namespace)
                    s"${objAttrRef.relatedServiceNamespace}.models.${objAttrRef.resourceType}"
                  else
                    objAttrRef.resourceType // namespace from the main service, plain model name
                  recurOps(modelToLookup) :+ objAttrRef
                case None =>
                  Nil
              }
          }
        case None if service.unions.exists(_.name == typ) =>
          val union = service.unions.find(_.name == typ).get
          union.types.flatMap(u => recurOps(u.`type`))
        case _ =>
          Nil
      }

    }

    val objRefAttrs = for {
      resource <- service.resources
      operation <- resource.operations
      dependantOps <- operation.body.map { body =>
        recurOps(body.`type`)
      }.getOrElse(Nil)
    } yield dependantOps

    val objRefAttrToOperation = objRefAttrs.map { objRefAttr =>
      val operationOpt = serviceNamespaceToResources.getOrElse(objRefAttr.relatedServiceNamespace, Nil)
        .find(_.`type` == objRefAttr.resourceType)
        .map(_.operations).getOrElse(Nil)
        .find(_.method == objRefAttr.operationMethod)

      (objRefAttr, operationOpt)
    }
      .collect {
        case (objRefAttr, Some(operation)) =>
          (objRefAttr, operation)
      }

    objRefAttrToOperation.distinct
  }

}
