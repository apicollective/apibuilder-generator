package generator

import io.apibuilder.spec.v0.models._
import io.postman.generator.attributes.v0.models.json._
import io.postman.generator.attributes.v0.models.{AttributeName, ObjectReference}
import lib.Datatype.Primitive
import models.AttributeValueReader
import models.service.ResolvedService
import org.scalactic.TripleEquals._
import models.attributes.PostmanAttributes._
import models.operation.DependantOperations
import play.api.Logging
import play.api.libs.json.{JsError, JsSuccess, JsValue, Reads}

import scala.reflect.{ClassTag, classTag}

object DependantOperationResolver extends Logging {

  import scala.languageFeature.implicitConversions

  /**
    * 1. Searches for special postman attributes [[ObjectReference]] in whole Service and it's imports.
    * 2. Recursively resolves a dependant operations for each specified attributes.
    * 3. Assigns a Postman variable name for each reference and changes the type to [[ExtendedObjectReference]]
    * 4. Returns a list of tuple from Extended Attribute References [[ExtendedObjectReference]] and dependant Operations [[DependantOperations]]
    *
    * For instance, 'buy_book_form' has a required 'book_id' param, that references to an existing book.
    * If a valid Apibuilder attribute is attached to 'book_id' param in the specification,
    * then this method will return a following tuple: (attribute, operation that creates a book)
    *
    * @param resolvedService - service with all imports at hand
    * @return a sequence of custom attribute to dependant operation pairs
    */
  def resolve(resolvedService: ResolvedService): Seq[(ExtendedObjectReference, DependantOperations)] = {

    val service: Service = resolvedService.service
    val serviceNamespaceToResources: Map[String, Seq[Resource]] = resolvedService.serviceNamespaceToResources

    val attributesFromResources: Seq[ExtendedObjectReference] = {
      for {
        resource <- service.resources
        foundedPathAttr <- resource.attributes if foundedPathAttr.name.equalsIgnoreCase(AttributeName.ObjectReference.toString)
        pureAttr <- tryAttributeReadsWithLogging[ObjectReference](foundedPathAttr.value)
        extendedObjectReference = pureAttr.toExtended
        parameterFromResourcePath = resource.path.flatMap(findParameterInPathString)
        postmanVariableNameOpt = parameterFromResourcePath.map(postmanVariableNameFrom)
      } yield {
        postmanVariableNameOpt.fold(extendedObjectReference) { pathVariableName =>
          extendedObjectReference.copy(postmanVariableName = pathVariableName)
        }
      }
    }

    val nestedAttributesFromResourceRefs = for {
      attribute <- attributesFromResources
      dependantOperations <- findOperationForAttribute(attribute, serviceNamespaceToResources).toSeq
      updatedReferencedOp = addNamespaceToOperationBodyIfNecessary(resolvedService, dependantOperations.referencedOperation, attribute.relatedServiceNamespace)
      body <- updatedReferencedOp.body.toSeq
      attribute <- deepSearchModelsForAttributes(body.`type`, service)
      extendedAttribute = attribute.toExtended
    } yield extendedAttribute

    val attributesFromModel = for {
      resource <- service.resources
      operation <- resource.operations
      body <- operation.body.toSeq
      attribute <- deepSearchModelsForAttributes(body.`type`, service)
      extendedAttribute = attribute.toExtended
    } yield extendedAttribute

    val allAttributes = (nestedAttributesFromResourceRefs ++ attributesFromResources ++ attributesFromModel).distinct

    for {
      attribute <- allAttributes
      dependantOperations <- findOperationForAttribute(attribute, serviceNamespaceToResources).toSeq
      updatedReferencedOp =
      addNamespaceToOperationBodyIfNecessary(resolvedService, dependantOperations.referencedOperation, attribute.relatedServiceNamespace)
      updatedDeleteOp = dependantOperations.deleteOperationOpt
        .map(addNamespaceToOperationBodyIfNecessary(resolvedService, _, attribute.relatedServiceNamespace))
      updatedDependantOperations = DependantOperations(updatedReferencedOp, updatedDeleteOp)
    } yield {
      (attribute, updatedDependantOperations)
    }
  }

  private def findReferenceAttributeInModelField(field: Field): Option[ObjectReference] = {
    AttributeValueReader.findAndReadFirst[ObjectReference](field.attributes, AttributeName.ObjectReference)
  }

  private def findOperationForAttribute(objRefAttr: ExtendedObjectReference, serviceNamespaceToResources: Map[String, Seq[Resource]]): Option[DependantOperations] = {
    serviceNamespaceToResources
      .get(key = objRefAttr.relatedServiceNamespace) match {
      case Some(resources) =>
        val operations =
          resources
            .filter(_.`type` === objRefAttr.resourceType)
            .flatMap(_.operations)

        val referencedOperationOpt = findOperationThatEndsWithOrEquals(operations, objRefAttr.operationMethod, objRefAttr.operationPath)

        val deleteOperationOpt = for {
          deleteOpPath <- objRefAttr.deleteOperationPath.map(_.toLowerCase)
          deleteOp <- findOperationThatEndsWithOrEquals(operations, Method.Delete, deleteOpPath)
        } yield deleteOp

        referencedOperationOpt.map { referencedOperation =>
          DependantOperations(referencedOperation, deleteOperationOpt)
        }
      case None =>
        logger.warn(s"ResolvedService namespace to resources map (includes imported services) does not contain key = ${objRefAttr.relatedServiceNamespace} / Can't find the referenced operation for $objRefAttr")
        None
    }
  }

  private def findOperationThatEndsWithOrEquals(operations: Seq[Operation], method: Method, pathSuffix: String): Option[Operation] = {
    operations.find { o =>
      o.method === method &&
        (o.path.toLowerCase.endsWith(pathSuffix.toLowerCase()) || o.path.equalsIgnoreCase(pathSuffix))
    }
  }

  private def deepSearchModelsForAttributes(typ: String, service: Service): Seq[ObjectReference] = {

    def recurSearch(typ: String): Seq[ObjectReference] = {
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

  private def findParameterInPathString(path: String): Option[Parameter] = {
    val firstParameterNameOpt = PathParamsFinder.find(path).headOption

    firstParameterNameOpt.map { paramName =>
      Parameter(
        name = paramName,
        `type` = Primitive.String.name,
        location = ParameterLocation.Path,
        required = true
      )
    }
  }

  private def addNamespaceToOperationBodyIfNecessary(resolvedService: ResolvedService, operation: Operation, referencedServiceNamespace: String): Operation = {
    operation.body match {
      case Some(body) if referencedServiceNamespace === resolvedService.service.namespace =>
        operation
      case Some(body) if !body.`type`.startsWith(referencedServiceNamespace) =>
        val typeToLookFor = body.`type`
        val enumOpt = resolvedService.service.enums.find(o => o.name.startsWith(referencedServiceNamespace) && o.name.contains(typeToLookFor)).map(_.name)
        val modelOpt = resolvedService.service.models.find(o => o.name.startsWith(referencedServiceNamespace) && o.name.contains(typeToLookFor)).map(_.name)
        val unionOpt = resolvedService.service.unions.find(o => o.name.startsWith(referencedServiceNamespace) && o.name.contains(typeToLookFor)).map(_.name)
        val newTypeSignature = unionOpt orElse modelOpt orElse enumOpt getOrElse {
          logger.warn(s"ResolvedService does not contain the type returned by the operation - $typeToLookFor")
          typeToLookFor
        }
        val updatedBody = body.copy(`type` = newTypeSignature)
        operation.copy(body = Some(updatedBody))
      case _ =>
        operation
    }
  }

  private def tryAttributeReadsWithLogging[A : Reads : ClassTag](json: JsValue): Option[A] = {
    val reads = implicitly[Reads[A]]
    reads.reads(json) match {
      case JsSuccess(entity, _) =>
        Some(entity)
      case JsError(errors) =>
        logger.warn(s"Attribute [${AttributeName.ObjectReference}] value $json could not be read as ${classTag[A].runtimeClass.getName} / Errors: $errors")
        None
    }
  }

}
