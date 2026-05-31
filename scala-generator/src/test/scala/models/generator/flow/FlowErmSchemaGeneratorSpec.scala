package models.generator.flow

import helpers.{ServiceHelpers, TestHelpers}
import io.apibuilder.generator.v0.models.{Attribute, InvocationForm}
import io.apibuilder.spec.v0.models.{Service, UnionType}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsString, Json}
import scala.generator.flow.erm.FlowErmSchemaGenerator

class FlowErmSchemaGeneratorSpec extends AnyFunSpec with Matchers with ServiceHelpers with TestHelpers {

  private lazy val domainService: Service = makeService(
    name = "widget_service",
    namespace = "io.flow.widget.v0",
    organization = makeOrganization(key = "flow"),
    application = makeApplication(key = "widget"),
    version = "0.0.1",
    models = Seq(
      makeModel(
        name = "label",
        plural = "labels",
        fields = Seq(makeField(name = "text", `type` = "string")),
      ),
      makeModel(
        name = "widget",
        plural = "widgets",
        fields = Seq(
          makeField(name = "id", `type` = "string"),
          makeField(name = "name", `type` = "string"),
          makeField(name = "label", `type` = "label"),
        ),
      ),
    ),
    unions = Seq(
      makeUnion(
        name = "widget_kind",
        plural = "widget_kinds",
        types = Seq(
          UnionType(`type` = "widget", discriminatorValue = Some("widget")),
        ),
      ),
    ),
  )

  private def typesAttribute(names: String*): Attribute =
    Attribute(name = "types", value = Json.stringify(JsArray(names.map(JsString(_)))))

  // ---------------------------------------------------------------------------

  it("generates a single file") {
    val form = InvocationForm(service = domainService)
    val files = rightOrErrors(FlowErmSchemaGenerator.invoke(form))
    files should have size 1
    files.head.name should endWith(".scala")
  }

  it("generates a GeneratedErmSchema object in the erm package") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("package io.flow.widget.v0.erm")
    c should include("object GeneratedErmSchema")
  }

  it("emits ErmSpec and apibuilder spec imports") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("import io.flow.event.relation.mapper.schema.ErmSpec")
    c should include("import io.apibuilder.spec.v0.models.{Field, Model, Union, UnionType}")
  }

  it("generates implicits for all primary-service models when no type filter is given") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("implicit val ermSpecWidget: ErmSpec[Widget]")
    c should include("implicit val ermSpecLabel: ErmSpec[Label]")
  }

  it("generates implicits for unions") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("implicit val ermSpecWidgetKind: ErmSpec[WidgetKind]")
    c should include("""ErmSpec.union(""")
    c should include("""qualifiedName = "io.flow.widget.v0.unions.widget_kind"""")
  }

  it("uses apibuilder qualified names with .models for models and .unions for unions") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("""qualifiedName = "io.flow.widget.v0.models.widget"""")
    c should include("""qualifiedName = "io.flow.widget.v0.unions.widget_kind"""")
  }

  it("embeds field name, type, and required flag") {
    val form = InvocationForm(service = domainService)
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("""Field(name = "id", `type` = "string", required = true)""")
  }

  it("respects the types attribute filter") {
    val form = InvocationForm(service = domainService, attributes = Seq(typesAttribute("widget")))
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("implicit val ermSpecWidget: ErmSpec[Widget]")
    c should not include "ermSpecWidgetKind"
  }

  it("returns an error for an unknown type name in the filter") {
    val form = InvocationForm(service = domainService, attributes = Seq(typesAttribute("nonexistent")))
    FlowErmSchemaGenerator.invoke(form) match {
      case Left(errors) => errors.mkString should include("'nonexistent'")
      case Right(_) => fail("Expected an error for an unknown type name")
    }
  }

  it("includes transitive model dependencies and places them before their dependents") {
    val form = InvocationForm(
      service = domainService,
      attributes = Seq(typesAttribute("widget")),
    )
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    // label is a dep of widget — must appear
    c should include("implicit val ermSpecLabel: ErmSpec[Label]")
    // label must appear before widget
    c.indexOf("ermSpecLabel") should be < c.indexOf("ermSpecWidget")
  }

  it("resolves types from imported services when listed in the filter") {
    val eventService: Service = makeService(
      name = "widget_event_service",
      namespace = "io.flow.widgetevent.v0",
      organization = makeOrganization(key = "flow"),
      application = makeApplication(key = "widget-events"),
      version = "0.0.1",
      imports = Seq(makeImport(domainService)),
      models = Seq(
        makeModel(
          name = "widget_upserted",
          plural = "widget_upserteds",
          fields = Seq(
            makeField(name = "event_id", `type` = "string"),
            makeField(name = "widget", `type` = "io.flow.widget.v0.models.widget"),
          ),
        ),
      ),
    )

    val form = InvocationForm(
      service = eventService,
      importedServices = Some(Seq(domainService)),
      attributes = Seq(typesAttribute("widget_upserted", "io.flow.widget.v0.models.widget")),
    )
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("implicit val ermSpecWidgetUpserted")
    c should include("implicit val ermSpecWidget")
  }

  it("includes union member models automatically without explicit listing") {
    val form = InvocationForm(
      service = domainService,
      attributes = Seq(typesAttribute("widget_kind")),
    )
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("implicit val ermSpecWidgetKind: ErmSpec[WidgetKind]")
    c should include("implicit val ermSpecWidget: ErmSpec[Widget]")
  }

  it("generates valid Scala source code") {
    val form = InvocationForm(service = domainService)
    val files = rightOrErrors(FlowErmSchemaGenerator.invoke(form))
    models.TestHelper.assertValidScalaSourceFiles(files)
  }

  it("preserves custom discriminator value from spec") {
    val serviceWithDiscriminator = domainService.copy(
      unions = Seq(
        makeUnion(
          name = "widget_kind",
          plural = "widget_kinds",
          types = Seq(UnionType(`type` = "widget", discriminatorValue = Some("widget"))),
        ).copy(discriminator = Some("kind")),
      ),
    )
    val form = InvocationForm(service = serviceWithDiscriminator, attributes = Seq(typesAttribute("widget_kind")))
    val c = rightOrErrors(FlowErmSchemaGenerator.invoke(form)).head.contents
    c should include("""discriminator = Some("kind")""")
  }
}
