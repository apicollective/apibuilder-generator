package generator

import akka.http.scaladsl.model.StatusCodes
import examples.ExampleJson
import generator.PostmanCollectionGenerator.Constants
import io.apibuilder.spec.v0.models.{Operation, Parameter, ParameterLocation}
import io.flow.postman.v0.models.{Folder, Item}
import models.attributes.PostmanAttributes.ExtendedObjectReference
import io.flow.postman.v0.{models => postman}
import lib.Datatype.Primitive
import models.operation.DependantOperations
import org.scalactic.TripleEquals._

object SetupCleanupFolderBuilder {

  /**
    * Prepares dependant entities setup and cleanup phases.
    *
    * @param objReferenceAttrToOperationTuples sequence of tuples, {{ExtendedObjectReference}} to {{DependantOperations}}.
    * @param serviceSpecificHeaders            Service-specific headers.
    * @param examplesProvider                  Examples generator.
    * @return Tuple of options of dependant entities setup and cleanup folders for Postman.
    */
  def prepareDependantEntitiesSetupAndCleanup(
    objReferenceAttrToOperationTuples: Seq[(ExtendedObjectReference, DependantOperations)],
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson
  ): (Option[Folder], Option[Folder]) = {

    val setupItemToCleanupItemOpts = objReferenceAttrToOperationTuples.map {
      case (objRefAttr, operation) =>

        val setupItem =
          prepareSetupOperation(objRefAttr, operation.referencedOperation, serviceSpecificHeaders, examplesProvider)
        val deleteItemOpt =
          operation
            .deleteOperationOpt
            .map(prepareDeleteOpWithFilledParam(objRefAttr, _, serviceSpecificHeaders, examplesProvider))

        setupItem -> deleteItemOpt
    }

    val setupSteps = setupItemToCleanupItemOpts.map(_._1)
    val cleanupSteps = setupItemToCleanupItemOpts.flatMap(_._2)

    val setupFolderOpt = wrapInFolder(setupSteps, Constants.EntitiesSetup)
    val cleanupFolderOpt = wrapInFolder(cleanupSteps, Constants.EntitiesCleanup)

    setupFolderOpt -> cleanupFolderOpt
  }

  private def addDependencyItemVarSetting(objRefAttr: ExtendedObjectReference, item: postman.Item, varNameOpt: Option[String]): postman.Item = {

    val varName = objRefAttr.postmanVariableName.name

    val successfulStatusCodes = Set(StatusCodes.OK.intValue, StatusCodes.Created.intValue)
    val rootJsonObjIsArray = item.response.flatMap(_.collectFirst {
      case resp if resp.code.exists(successfulStatusCodes.contains) =>
        resp.body.exists(t => t.trim.startsWith("[") && t.trim.endsWith("]"))
    }).getOrElse(false)
    val jsonBase = if (rootJsonObjIsArray) "jsonData[0]" else "jsonData"

    val scriptExecFragment = Seq(
      """var jsonData = JSON.parse(responseBody);""",
      s"""var id = $jsonBase.${objRefAttr.identifierField};""",
      s"""if (id != null) pm.environment.set("$varName", id);"""
    )

    val events = item.event.getOrElse(Seq.empty)

    events
      .find(_.listen === postman.EventType.Test) match {
      case Some(testEvent) =>
        val updatedScript = testEvent.script.map(_.copy(
          exec = testEvent.script.map(_.exec ++ scriptExecFragment).getOrElse(Seq.empty)
        ))
        val updatedTestEvent = testEvent.copy(script = updatedScript)
        val updatedEvents =
          events.filterNot(_.listen === postman.EventType.Test) :+ updatedTestEvent
        item.copy(event = Some(updatedEvents))
      case None =>
        val eventToAdd = postman.Event(
          listen = postman.EventType.Test,
          script = Some(postman.Script(
            exec = scriptExecFragment
          ))
        )
        item.copy(event = Some(Seq(eventToAdd)))
    }
  }

  private def prepareSetupOperation(
    objRefAttr: ExtendedObjectReference,
    referencedOperation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson): Item = {

    val filledReferencedOperation = objRefAttr.queryParams match {
      case Some(queryParams) if queryParams.nonEmpty =>
        fillQueryParams(referencedOperation, queryParams)
      case _ => referencedOperation
    }

    val setupItem = PostmanItemBuilder.build(filledReferencedOperation, serviceSpecificHeaders, examplesProvider, None)
    val setupItemWithTests = PredefinedCollectionItems.addItemTests(setupItem)
    val setupWithTestsAndVar = addDependencyItemVarSetting(objRefAttr, setupItemWithTests, None)

    setupWithTestsAndVar
  }

  private def fillQueryParams(operation: Operation, rawQueryParamMap: Map[String, String]): Operation = {
    val (queryParameters, otherParams) =
      operation
        .parameters
        .partition(_.location === ParameterLocation.Query)

    val filledQueryParams = rawQueryParamMap.map {
      case (key, value) =>
        val targetQueryParamOpt = queryParameters.find(_.name equalsIgnoreCase key)
        targetQueryParamOpt match {
          case Some(queryParam) =>
            queryParam.copy(
              example = Some(value),
              required = true
            )
          case None =>
            Parameter(
              name = key,
              `type` = Primitive.String.name,
              location = ParameterLocation.Query,
              example = Some(value),
              required = true
            )
        }
    }.toSeq

    operation.copy(parameters = filledQueryParams ++ otherParams)
  }

  private def prepareDeleteOpWithFilledParam(
    objRefAttr: ExtendedObjectReference,
    deleteOperation: Operation,
    serviceSpecificHeaders: Seq[postman.Header],
    examplesProvider: ExampleJson): Item = {

    val (pathParameters, otherParams) = deleteOperation.parameters.partition(_.location === ParameterLocation.Path)
    val filledPathParameters = pathParameters match {
      case params if pathParameters.nonEmpty =>
        val updatedLast = params.lastOption.get.copy(example = Some(objRefAttr.postmanVariableName.reference))
        params.init :+ updatedLast
      case _ =>
        val rawPathParamOpt = findLastPathParam(deleteOperation.path)

        rawPathParamOpt.map { rawPathParam =>
          Parameter(
            name = rawPathParam,
            `type` = Primitive.String.name,
            location = ParameterLocation.Path,
            example = Some(objRefAttr.postmanVariableName.reference),
            required = true
          )
        }.toSeq
    }
    val filledDeleteOp = deleteOperation.copy(parameters = filledPathParameters ++ otherParams)

    val postmanItem = PostmanItemBuilder.build(filledDeleteOp, serviceSpecificHeaders, examplesProvider, None)
    PredefinedCollectionItems.addItemTests(postmanItem)
  }

  private def findLastPathParam(path: String): Option[String] = {
    path
      .split('/')
      .reverse
      .find(_.startsWith(":"))
      .map(_.stripPrefix(":"))
  }


  private def wrapInFolder(items: Seq[Item], folderName: String): Option[Folder] = {
    if (items.nonEmpty) {
      Some(
        postman.Folder(
          name = folderName,
          item = items
        )
      )
    } else None
  }

}
