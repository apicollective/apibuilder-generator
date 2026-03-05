package generator.openapi

import io.apibuilder.spec.v0.models.Service
import io.apibuilder.validation.{ApiBuilderType, MultiService}

import scala.annotation.tailrec

case class MultiServiceExtractor(
  sourceMultiService: MultiService,
) {

  private val ListRx = "^\\[(.*)\\]$".r
  private val MapRx = "^map\\[(.*)\\]$".r

  def extractReferencedTypes(service: Service): MultiService = {
    val ns = service.namespace

    val definedTypes = service.models.map(_.name) ++
      service.enums.map(_.name) ++
      service.unions.map(_.name) ++
      service.interfaces.map(_.name)

    val fieldTypes = service.models.flatMap(_.fields.map(_.`type`))
    val interfaceFieldTypes = service.interfaces.flatMap(_.fields.map(_.`type`))
    val unionMemberTypes = service.unions.flatMap(_.types.map(_.`type`))
    val resourceTypes = service.resources.flatMap { r =>
      Seq(r.`type`) ++ r.operations.flatMap { op =>
        op.body.map(_.`type`).toSeq ++
          op.responses.map(_.`type`) ++
          op.parameters.map(_.`type`)
      }
    }

    val allRefs = (definedTypes ++ fieldTypes ++ interfaceFieldTypes ++ unionMemberTypes ++ resourceTypes)
      .map(stripWrapper)
      .distinct

    val seeds = allRefs.flatMap { ref =>
      sourceMultiService.findType(ns, ref).collect { case t: ApiBuilderType => t }
    }.toList

    val types = collectAll(seeds, Set.empty)
    buildMultiService(types)
  }

  private def childTypeRefs(t: ApiBuilderType): Seq[(String, String)] = t match {
    case m: ApiBuilderType.Model =>
      m.model.fields.map(f => stripWrapper(f.`type`) -> m.namespace)
    case u: ApiBuilderType.Union =>
      u.union.types.map(ut => ut.`type` -> u.namespace)
    case i: ApiBuilderType.Interface =>
      i.interface.fields.map(f => stripWrapper(f.`type`) -> i.namespace)
    case _: ApiBuilderType.Enum => Nil
  }

  @tailrec
  private def collectAll(pending: List[ApiBuilderType], acc: Set[ApiBuilderType]): Set[ApiBuilderType] = {
    pending match {
      case Nil => acc
      case head :: tail if acc.exists(_.qualified == head.qualified) =>
        collectAll(tail, acc)
      case head :: tail =>
        val children = childTypeRefs(head).flatMap { case (name, ns) =>
          sourceMultiService.findType(ns, name).collect { case t: ApiBuilderType => t }
        }
        collectAll(children.toList ::: tail, acc + head)
    }
  }

  @tailrec
  private def stripWrapper(typeName: String): String = {
    typeName match {
      case ListRx(inner) => stripWrapper(inner)
      case MapRx(inner) => stripWrapper(inner)
      case other => other
    }
  }

  private def buildMultiService(types: Set[ApiBuilderType]): MultiService = {
    import io.apibuilder.validation.ApiBuilderService

    val services = types
      .groupBy(_.service.namespace)
      .toList
      .map { case (_, nsTypes) =>
        val base = nsTypes.head.service.service.copy(
          models = Nil,
          enums = Nil,
          interfaces = Nil,
          unions = Nil,
          resources = Nil,
          headers = Nil,
          imports = Nil,
        )
        val service = nsTypes.foldLeft(base) { (svc, t) =>
          t match {
            case m: ApiBuilderType.Model => svc.copy(models = m.model +: svc.models)
            case e: ApiBuilderType.Enum => svc.copy(enums = e.`enum` +: svc.enums)
            case i: ApiBuilderType.Interface => svc.copy(interfaces = i.interface +: svc.interfaces)
            case u: ApiBuilderType.Union => svc.copy(unions = u.union +: svc.unions)
          }
        }
        ApiBuilderService(service)
      }

    MultiService(services)
  }
}
