package scala.generator.flow.erm

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models.{Model, Service, Union}
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
    val discriminator: String = union.discriminator.getOrElse("discriminator")
  }

  override def invoke(form: InvocationForm): Either[Seq[String], Seq[File]] = {
    val allServices = form.service +: form.importedServices.getOrElse(Nil)
    val typeFilter  = parseTypeFilter(form)

    resolveSeedTypes(form, allServices, typeFilter).map { case (seedModels, seedUnions) =>
      val unionMemberModels =
        seedUnions.flatMap(ru => ru.union.types.flatMap(ut => findModel(ut.`type`, allServices)))
      val allModels =
        collectTransitive((seedModels ++ unionMemberModels).distinctBy(_.qualifiedName), allServices)
      val header = ApiBuilderComments(form.service.version, form.userAgent).toJavaString + "\n"

      Seq(
        ServiceFileNames.toFile(
          form.service.namespace,
          form.service.organization.key,
          form.service.application.key,
          form.service.version,
          "ErmSchema",
          header + generateContent(form.service, allModels, seedUnions),
          Some("Scala"),
        )
      )
    }
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

  // ---------------------------------------------------------------------------
  // Code generation

  private def generateContent(
    service: Service,
    models: Seq[ResolvedModel],
    unions: Seq[ResolvedUnion],
  ): String = {
    val ermPackage = Namespaces(service.namespace).base + ".erm"
    val sb = new StringBuilder

    sb.append(s"package $ermPackage\n\n")
    sb.append(
      "import io.flow.event.relation.mapper.schema." +
        "{ErmSpec, ErmModelSpec, ErmFieldSpec, ErmUnionSpec, ErmUnionMemberSpec}\n",
    )

    val imports = (models.map(_.scalaImport) ++ unions.map(_.scalaImport)).distinct.sorted
    imports.foreach(i => sb.append(s"import $i\n"))

    sb.append(s"\nobject GeneratedErmSchema {\n\n")

    models.foreach { rm =>
      val valName = s"ermSpec${rm.scalaClassName}"
      sb.append(s"  implicit val $valName: ErmSpec[${rm.scalaClassName}] =\n")
      sb.append(s"    ErmSpec.model(\n")
      sb.append(s"""      qualifiedName = "${rm.qualifiedName}",\n""")
      sb.append(s"""      spec = ErmModelSpec("${rm.model.name}", Seq(\n""")
      rm.model.fields.foreach { f =>
        sb.append(s"""        ErmFieldSpec("${f.name}", "${f.`type`}", required = ${f.required}),\n""")
      }
      sb.append("      )),\n    )\n\n")
    }

    unions.foreach { ru =>
      val valName = s"ermSpec${ru.scalaClassName}"
      sb.append(s"  implicit val $valName: ErmSpec[${ru.scalaClassName}] =\n")
      sb.append(s"    ErmSpec.union(\n")
      sb.append(s"""      qualifiedName = "${ru.qualifiedName}",\n""")
      sb.append(
        s"""      spec = ErmUnionSpec("${ru.union.name}", discriminator = "${ru.discriminator}", members = Seq(\n""",
      )
      ru.union.types.foreach { ut =>
        val dv = ut.discriminatorValue.getOrElse(ut.`type`)
        sb.append(s"""        ErmUnionMemberSpec("${ut.`type`}", "$dv"),\n""")
      }
      sb.append("      )),\n    )\n\n")
    }

    sb.append("}\n")
    sb.toString.stripTrailing()
  }
}
