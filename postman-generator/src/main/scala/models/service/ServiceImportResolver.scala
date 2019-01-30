package models.service

import io.apibuilder.spec.v0.models.{Enum, Model, Resource, Service, Union}

object ServiceImportResolver {

  type ImportedObjects = (Seq[Enum], Seq[Model], Seq[Union], Map[String, Seq[Resource]])

  // TODO: investigate if this mechanism (from api-examples-poc repo) is valid
  // another approach is visible in apibuilder repo, Versions.scala file
  // (it's possible that enum/model namespaces and names are created with a bug)
  def resolveService(service: Service, importedServices: Seq[Service]): ResolvedService = {

    val (importedEnums, importedModels, importedUnions, importedServiceNamespaceToResourceMap): ImportedObjects =
      fetchImportedObjects(importedServices)

    val serviceNamespaceToResourcesMap = importedServiceNamespaceToResourceMap ++ Map(service.namespace -> service.resources)

    val serviceWithResolvedImports = service.copy(
      imports = Seq.empty,
      enums = service.enums ++ importedEnums,
      models = service.models ++ importedModels,
      unions = service.unions ++ importedUnions
    )

    ResolvedService(
      service = serviceWithResolvedImports,
      serviceNamespaceToResources = serviceNamespaceToResourcesMap
    )
  }

  private def fetchImportedObjects(importedServices: Seq[Service]): ImportedObjects = {

    def resolveRecursive(
      importedServices: Seq[Service],
      importedObjectsAcc: ImportedObjects
    ): ImportedObjects = {
      importedServices.headOption match {
        case None => importedObjectsAcc
        case Some(importedService) =>
          val importedServiceNamespace = importedService.namespace

          val enums = importedService.enums.map { enum =>
            enum.copy(name = s"$importedServiceNamespace.enums.${enum.name}")
          }

          def isFieldTypeEnumReference(fieldType: String): Boolean = importedService.enums.map(_.name).contains(fieldType)

          def isFieldTypeModelReference(fieldType: String): Boolean = importedService.models.map(_.name).contains(fieldType)

          def isFieldTypeUnionReference(fieldType: String): Boolean = importedService.unions.map(_.name).contains(fieldType)

          val models = importedService.models.map { model =>
            val updatedFields = model.fields.map {
              case field if isFieldTypeEnumReference(field.`type`) =>
                field.copy(`type` = s"$importedServiceNamespace.enums.${field.`type`}")
              case field if isFieldTypeModelReference(field.`type`) =>
                field.copy(`type` = s"$importedServiceNamespace.models.${field.`type`}")
              case field if isFieldTypeUnionReference(field.`type`) =>
                field.copy(`type` = s"$importedServiceNamespace.unions.${field.`type`}")
              case otherField => otherField
            }
            model.copy(
              fields = updatedFields,
              name = s"$importedServiceNamespace.models.${model.name}"
            )
          }

          val unions = importedService.unions.map { union =>
            union.copy(name = s"$importedServiceNamespace.unions.${union.name}")
          }

          val serviceNamespaceToResources = importedService.namespace -> importedService.resources

          (
            importedObjectsAcc._1 ++ enums,
            importedObjectsAcc._2 ++ models,
            importedObjectsAcc._3 ++ unions,
            importedObjectsAcc._4 + serviceNamespaceToResources
          )
      }
    }

    resolveRecursive(
      importedServices = importedServices,
      importedObjectsAcc = (Seq.empty, Seq.empty, Seq.empty, Map.empty)
    )
  }


}
