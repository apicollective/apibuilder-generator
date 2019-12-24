package generator

import io.apibuilder.spec.v0.models.{Enum, Field, Model, Resource, Service, Union, UnionType}
import models.service.ResolvedService

object ServiceImportResolver {

  type ImportedObjects = (Seq[Enum], Seq[Model], Seq[Union], Map[String, Seq[Resource]])

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

          val models = importedService.models.map { model =>
            model.copy(
              name = s"$importedServiceNamespace.models.${model.name}",
              fields = model.fields.map(prefixModelFields(_: Field)(importedService))
            )
          }

          val unions = importedService.unions.map { union =>
            union.copy(
              name = s"$importedServiceNamespace.unions.${union.name}",
              types = union.types.map(prefixUnionType(_: UnionType)(importedService))
            )
          }

          val serviceNamespaceToResources = importedService.namespace -> importedService.resources

          val newAcc = (
            importedObjectsAcc._1 ++ enums,
            importedObjectsAcc._2 ++ models,
            importedObjectsAcc._3 ++ unions,
            importedObjectsAcc._4 + serviceNamespaceToResources
          )

          resolveRecursive(importedServices.tail, newAcc)
      }
    }

    resolveRecursive(
      importedServices = importedServices,
      importedObjectsAcc = (Seq.empty, Seq.empty, Seq.empty, Map.empty)
    )
  }

  def fixType[T](origTyp: String, update: String => T)(importedService: Service): T = {
    val importNamespace = importedService.namespace

    val typeRx = """(\[?)(.*?)(\]?)""".r
    origTyp match {
      case typeRx(prefix, typ, suffix) =>
        if (importedService.models.exists(_.name == typ)) {
          update(s"$prefix$importNamespace.models.$typ$suffix")
        } else if (importedService.enums.exists(_.name == typ)) {
          update(s"$prefix$importNamespace.enums.$typ$suffix")
        } else if (importedService.unions.exists(_.name == typ)) {
          update(s"$prefix$importNamespace.unions.$typ$suffix")
        } else {
          update(origTyp)
        }
      case _ => update(origTyp)
    }
  }

  def prefixModelFields(field: Field) = fixType(field.`type`, typ => field.copy(`type` = typ)) _

  def prefixUnionType(unionType: UnionType) = fixType(unionType.`type`, typ => unionType.copy(`type` = typ)) _
}
