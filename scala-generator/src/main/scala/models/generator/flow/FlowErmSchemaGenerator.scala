package scala.generator.flow.erm

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Enum, Model, Service, Union}
import generator.ServiceFileNames
import lib.generator.CodeGenerator
import scala.generator.{Namespaces, ScalaUtil}
import scala.models.ApiBuilderComments
import play.api.libs.json.{JsArray, JsString, Json}

/** Generates ErmSpec[T] implicits for all (or a named subset of) models and unions in an
  * apibuilder service spec.  The generated object is imported at DSL call sites to provide
  * compile-time schema binding for lib-event-relation-mapper.
  *
  * Self-contained: all generation logic lives here.  The only host-app coupling is the
  * CodeGenTarget entry in Generators.scala — move this object to a standalone service
  * without modification.
  */
object FlowErmSchemaGenerator extends CodeGenerator {

  private val Primitives: Set[String] = Set(
    "string", "integer", "long", "boolean", "double", "decimal",
    "uuid", "date-time-iso8601", "date-iso8601", "object", "unit", "json",
  )

  private case class ResolvedModel(service: Service, model: Model) {
    val qualifiedName: String = s"${service.namespace}.models.${model.name}"
    val scalaClassName: String = ScalaUtil.toLocalClassName(model.name)
    val scalaImport: String = s"${service.namespace}.models.$scalaClassName"
  }

  private case class ResolvedUnion(service: Service, union: Union) {
    val qualifiedName: String = s"${service.namespace}.unions.${union.name}"
    val scalaClassName: String = ScalaUtil.toLocalClassName(union.name)
    val scalaImport: String = s"${service.namespace}.models.$scalaClassName"
  }

  private case class ResolvedEnum(service: Service, `enum`: Enum) {
    val qualifiedName: String = s"${service.namespace}.enums.${`enum`.name}"
    val scalaClassName: String = ScalaUtil.toLocalClassName(`enum`.name)
    val scalaImport: String = s"${service.namespace}.models.$scalaClassName"
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val allServices = form.service +: form.importedServices.getOrElse(Nil)
    val typeFilter  = parseTypeFilter(form)

    resolveSeedTypes(form, allServices, typeFilter).map { case (seedModels, seedUnions) =>
      val unionMemberModels =
        seedUnions.flatMap(ru => ru.union.types.flatMap(ut => findModel(ut.`type`, allServices)))
      val allModels =
        collectTransitive((seedModels ++ unionMemberModels).distinctBy(_.qualifiedName), allServices)
      val allEnums = collectEnums(form.service.namespace, allModels, allServices)
      val header = ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"

      val schemaFile = ServiceFileNames.toFile(
        form.service.namespace,
        form.service.organization.key,
        form.service.application.key,
        form.service.version,
        "ErmSchema",
        header + generateContent(form.service, allModels, seedUnions, allEnums),
        Some("Scala"),
      )

      // When `emit_aggregator=true`, also emit a single `io.flow.erm.GeneratedAllErmSpecs`
      // object aggregating every `<ns>.erm.GeneratedErmSchema.all` reachable from the primary
      // service + its imported services. The service module then just imports that one symbol —
      // no hand-written list.
      val aggregatorFile = if (aggregatorEnabled(form)) {
        val namespaces = (form.service.namespace +: form.importedServices.getOrElse(Nil).map(_.namespace)).distinct.sorted
        Some(
          File(
            name = "GeneratedAllErmSpecs.scala",
            dir = Some("io/flow/erm"),
            contents = header + generateAggregator(namespaces),
          )
        )
      } else None

      Seq(schemaFile) ++ aggregatorFile.toSeq
    }
  }

  private def aggregatorEnabled(form: InvocationForm): Boolean =
    form.attributes.find(_.name == "emit_aggregator").exists(_.value.trim.toLowerCase == "true")

  private def generateAggregator(namespaces: Seq[String]): String = {
    val refs = namespaces.map(ns => s"$ns.erm.GeneratedErmSchema.all")
    val body = refs.map(r => s"      $r").mkString(" ++\n")
    s"""package io.flow.erm
       |
       |import io.flow.apibuilder.MultiServiceProvider
       |import io.flow.event.relation.mapper.builder.SynthesizedMultiServiceProvider
       |import io.flow.event.relation.mapper.schema.ErmSpec
       |import play.api.inject.Module
       |import play.api.{Configuration, Environment}
       |
       |import javax.inject.Singleton
       |
       |/** Aggregates every per-apibuilder-service `GeneratedErmSchema.all` reachable from the
       |  * primary service that emitted this file (plus its imported services). The service ERM
       |  * module then just enables `io.flow.erm.GeneratedErmModule` — no hand-written list,
       |  * no service-side SynthesizedMultiServiceProvider construction.
       |  */
       |object GeneratedAllErmSpecs {
       |  val all: Seq[ErmSpec[_]] =
       |$body
       |}
       |
       |@Singleton
       |final class GeneratedSynthesizedMultiServiceProvider
       |  extends SynthesizedMultiServiceProvider(GeneratedAllErmSpecs.all)
       |
       |class GeneratedErmModule extends Module {
       |  def bindings(env: Environment, conf: Configuration) =
       |    Seq(bind[MultiServiceProvider].to[GeneratedSynthesizedMultiServiceProvider])
       |}
       |""".stripMargin
  }

  private def parseTypeFilter(form: InvocationForm): Option[Set[String]] =
    form.attributes
      .find(_.name == "types")
      .flatMap(attr => scala.util.Try(Json.parse(attr.value)).toOption.collect {
        case arr: JsArray => arr.value.collect { case JsString(s) => s }.toSet
      })

  private def resolveSeedTypes(
    form: InvocationForm,
    allServices: Seq[Service],
    typeFilter: Option[Set[String]],
  ): Either[Seq[String], (Seq[ResolvedModel], Seq[ResolvedUnion])] = typeFilter match {
    case None =>
      Right((
        form.service.models.map(ResolvedModel(form.service, _)),
        form.service.unions.map(ResolvedUnion(form.service, _)),
      ))

    case Some(names) =>
      val errors = names.toSeq.collect {
        case name if findModel(name, allServices).isEmpty && findUnion(name, allServices).isEmpty =>
          s"Type '$name' not found as a model or union in the spec or its imports"
      }
      if (errors.nonEmpty)
        Left(errors)
      else
        Right((
          names.toSeq.flatMap(findModel(_, allServices)),
          names.toSeq.flatMap(findUnion(_, allServices)),
        ))
  }

  // ---------------------------------------------------------------------------
  // Resolution

  private def findModel(name: String, services: Seq[Service]): Option[ResolvedModel] =
    services.iterator.flatMap { svc =>
      svc.models
        .find(m => m.name == name || s"${svc.namespace}.models.${m.name}" == name)
        .map(ResolvedModel(svc, _))
    }.nextOption()

  private def findUnion(name: String, services: Seq[Service]): Option[ResolvedUnion] =
    services.iterator.flatMap { svc =>
      svc.unions
        .find(u => u.name == name || s"${svc.namespace}.unions.${u.name}" == name)
        .map(ResolvedUnion(svc, _))
    }.nextOption()

  private def findEnum(name: String, services: Seq[Service]): Option[ResolvedEnum] =
    services.iterator.flatMap { svc =>
      svc.enums
        .find(e => e.name == name || s"${svc.namespace}.enums.${e.name}" == name)
        .map(ResolvedEnum(svc, _))
    }.nextOption()

  private def directModelDeps(rm: ResolvedModel, services: Seq[Service]): Seq[ResolvedModel] =
    rm.model.fields.flatMap(f => extractModelTypeName(f.`type`).flatMap(findModel(_, services)))

  private def extractModelTypeName(fieldType: String): Option[String] = {
    val inner = fieldType
      .stripPrefix("[").stripSuffix("]")
      .replaceFirst("""^map\[""", "").stripSuffix("]")
    if (Primitives.contains(inner)) None else Some(inner)
  }

  // ---------------------------------------------------------------------------
  // Transitive collection + topological sort

  private def collectTransitive(seeds: Seq[ResolvedModel], services: Seq[Service]): Seq[ResolvedModel] = {
    val visited = scala.collection.mutable.LinkedHashMap[String, ResolvedModel]()
    val queue   = scala.collection.mutable.Queue(seeds: _*)
    while (queue.nonEmpty) {
      val rm = queue.dequeue()
      if (!visited.contains(rm.qualifiedName)) {
        visited(rm.qualifiedName) = rm
        directModelDeps(rm, services)
          .filterNot(d => visited.contains(d.qualifiedName))
          .foreach(queue.enqueue(_))
      }
    }
    topologicalSort(visited.values.toSeq, services)
  }

  private def topologicalSort(models: Seq[ResolvedModel], services: Seq[Service]): Seq[ResolvedModel] = {
    val byKey  = models.map(rm => rm.qualifiedName -> rm).toMap
    val result = scala.collection.mutable.ArrayBuffer[ResolvedModel]()
    val done   = scala.collection.mutable.Set[String]()
    val stack  = scala.collection.mutable.Set[String]()

    def visit(rm: ResolvedModel): Unit = {
      if (!done(rm.qualifiedName) && !stack(rm.qualifiedName)) {
        stack += rm.qualifiedName
        directModelDeps(rm, services).flatMap(d => byKey.get(d.qualifiedName)).foreach(visit)
        stack -= rm.qualifiedName
        done  += rm.qualifiedName
        result += rm
      }
    }

    models.foreach(visit)
    result.toSeq
  }

  /** Collect every enum reference visible to the primary service's namespace.
    *
    * Walks both the transitively-collected primary-namespace models AND every model in every
    * imported service. From all those field references, returns each enum whose namespace is the
    * primary service's. This way an enum like `io.flow.common.v0.enums.day_of_week`, referenced
    * only by a model in some OTHER service, still gets emitted into common's ErmSchema when common
    * is generated with that other service supplied as an import.
    */
  private def collectEnums(
    primaryNamespace: String,
    primaryModels: Seq[ResolvedModel],
    services: Seq[Service],
  ): Seq[ResolvedEnum] = {
    val visited = scala.collection.mutable.LinkedHashMap[String, ResolvedEnum]()
    val sourceModels: Seq[ResolvedModel] =
      primaryModels ++ services.flatMap(svc => svc.models.map(ResolvedModel(svc, _)))
    sourceModels.foreach { rm =>
      rm.model.fields.foreach { f =>
        extractModelTypeName(f.`type`).flatMap(findEnum(_, services)).foreach { re =>
          if (re.service.namespace == primaryNamespace && !visited.contains(re.qualifiedName)) {
            visited(re.qualifiedName) = re
          }
        }
      }
    }
    visited.values.toSeq
  }

  // ---------------------------------------------------------------------------
  // Code generation

  private def generateContent(
    service: Service,
    models: Seq[ResolvedModel],
    unions: Seq[ResolvedUnion],
    enums: Seq[ResolvedEnum],
  ): String = {
    val ermPackage = Namespaces(service.namespace).base + ".erm"
    val sb = new StringBuilder

    sb.append(s"package $ermPackage\n\n")
    sb.append("import io.flow.event.relation.mapper.schema.ErmSpec\n")
    sb.append("import io.apibuilder.spec.v0.models.{Enum, EnumValue, Field, Model, Union, UnionType}\n")

    val imports = (models.map(_.scalaImport) ++ unions.map(_.scalaImport) ++ enums.map(_.scalaImport)).distinct.sorted
    imports.foreach(i => sb.append(s"import $i\n"))

    sb.append(s"\nobject GeneratedErmSchema {\n\n")

    models.foreach { rm =>
      val valName = s"ermSpec${rm.scalaClassName}"
      sb.append(s"  implicit lazy val $valName: ErmSpec[${rm.scalaClassName}] =\n")
      sb.append(s"    ErmSpec.model(\n")
      sb.append(s"""      qualifiedName = "${rm.qualifiedName}",\n""")
      sb.append(s"""      spec = Model(name = "${rm.model.name}", plural = "${rm.model.plural}", fields = Seq(\n""")
      rm.model.fields.foreach { f =>
        sb.append(s"""        Field(name = "${f.name}", `type` = "${f.`type`}", required = ${f.required}),\n""")
      }
      sb.append("      )),\n    )\n\n")
    }

    unions.foreach { ru =>
      val valName = s"ermSpec${ru.scalaClassName}"
      val discriminatorExpr = ru.union.discriminator.fold("None")(d => s"""Some("$d")""")
      sb.append(s"  implicit lazy val $valName: ErmSpec[${ru.scalaClassName}] =\n")
      sb.append(s"    ErmSpec.union(\n")
      sb.append(s"""      qualifiedName = "${ru.qualifiedName}",\n""")
      sb.append(
        s"""      spec = Union(name = "${ru.union.name}", plural = "${ru.union.plural}", discriminator = $discriminatorExpr, types = Seq(\n""",
      )
      ru.union.types.foreach { ut =>
        val dv = ut.discriminatorValue.getOrElse(ut.`type`)
        sb.append(s"""        UnionType(`type` = "${ut.`type`}", discriminatorValue = Some("$dv")),\n""")
      }
      sb.append("      )),\n    )\n\n")
    }

    enums.foreach { re =>
      val valName = s"ermSpec${re.scalaClassName}"
      sb.append(s"  implicit lazy val $valName: ErmSpec[${re.scalaClassName}] =\n")
      sb.append(s"    ErmSpec.`enum`(\n")
      sb.append(s"""      qualifiedName = "${re.qualifiedName}",\n""")
      sb.append(s"""      spec = Enum(name = "${re.`enum`.name}", plural = "${re.`enum`.plural}", values = Seq(\n""")
      re.`enum`.values.foreach { ev =>
        sb.append(s"""        EnumValue(name = "${ev.name}"),\n""")
      }
      sb.append("      )),\n    )\n\n")
    }

    val allNames =
      models.map(rm => s"ermSpec${rm.scalaClassName}") ++
        unions.map(ru => s"ermSpec${ru.scalaClassName}") ++
        enums.map(re => s"ermSpec${re.scalaClassName}")
    sb.append("  val all: Seq[ErmSpec[_]] = Seq(\n")
    allNames.foreach(n => sb.append(s"    $n,\n"))
    sb.append("  )\n")

    sb.append("}\n")
    sb.toString.stripTrailing()
  }
}
