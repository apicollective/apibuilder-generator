package go.models

import io.apibuilder.spec.v0.models.{Application, Import, Organization}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ImportBuilderSpec extends AnyFunSpec with Matchers {

  def buildImports(
    namespace: String,
    enums: Seq[String] = Nil,
    unions: Seq[String] = Nil,
    models: Seq[String] = Nil
  ): Seq[Import] = {
    Seq(
      Import(
        uri = "http://apidoc.me/test/app/0.0.1/service.json",
        namespace = namespace,
        organization = Organization("test"),
        application = Application("app"),
        version = "0.0.1",
        enums = enums,
        unions = unions,
        models = models
      )
    )
  }

  it("parseMappings") {
    ImportBuilder.parseMappings(None) should be(Map())
    ImportBuilder.parseMappings(Some("")) should be(Map())
    ImportBuilder.parseMappings(Some("io.flow:github.com/flowcommerce/apidoc")) should be(
      Map("io.flow" -> "github.com/flowcommerce/apidoc")
    )
    ImportBuilder.parseMappings(Some("io.flow:bar me.apidoc:github.com/flowcommerce/go")) should be(
      Map("io.flow" -> "bar", "me.apidoc" -> "github.com/flowcommerce/go")
    )
  }

  it("empty if no imports") {
    val builder = ImportBuilder(None)
    builder.generate() should be("")
  }

  it("prefixes with package name") {
    val builder = ImportBuilder(None)

    builder.ensureImport("json")
    builder.ensureImport("encoding/json")
    builder.ensureImport("other/json")

    builder.generate() should be("""
import (
	encodingJson "encoding/json"
	"json"
	otherJson "other/json"
)
""".trim)
  }

  it("idempotent") {
    val builder = ImportBuilder(None)
    builder.ensureImport("io")
    builder.ensureImport("io")
    builder.generate() should be("""
import (
	"io"
)
""".trim)
  }

  it("alphabetizes") {
    val builder = ImportBuilder(None)
    builder.ensureImport("io")
    builder.ensureImport("fmt")
    builder.ensureImport("net/http")
    builder.generate() should be("""
import (
	"fmt"
	"io"
	"net/http"
)
""".trim)
  }

  it("aliases if needed") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))
    builder.ensureImport("io.flow.common.v0.models")
    builder.generate() should be("""
import (
	"github.com/flowcommerce/apidoc/common"
)
""".trim)
  }

  it("builds proper import paths") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))
    builder.ensureImport("io.flow.common.v0.models")
    builder.ensureImport("me.apidoc.spec.v0.models")
    builder.generate() should be("""
import (
	"github.com/flowcommerce/apidoc/common"
	"me/apidoc/spec"
)
""".trim)
  }

  it("aliases duplicate imports") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))
    builder.ensureImport("common")
    builder.ensureImport("net/common")
    builder.generate() should be("""
import (
	"common"
	netCommon "net/common"
)
""".trim)
  }

  it("aliases duplicate imports with org name when available") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))
    builder.ensureImport("common")
    builder.ensureImport("io.flow.common.v0.models")
    builder.generate() should be("""
import (
	"common"
	flowcommerceCommon "github.com/flowcommerce/apidoc/common"
)
""".trim)
  }

  it("resolves multi package imports") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))

    builder.ensureImport("io.flow.carrier.account.v0.unions.expandable_carrier_account")

    builder.generate() should be("""
import (
	carrierAccount "github.com/flowcommerce/apidoc/carrier/account"
)
""".trim)
  }
  
  it("squashes underscores") {
    val builder = ImportBuilder(Some("io.flow:github.com/flowcommerce/apidoc"))

    builder.ensureImport("io.flow.service_level.v0.models.service_level")
    builder.ensureImport("io.flow.service.level.v0.models.service_level")
    builder.ensureImport("io.flow.servicelevel.v0.models.service_level")

    builder.generate() should be("""
import (
	flowcommerceServiceLevel "github.com/flowcommerce/apidoc/service/level"
	serviceLevel "github.com/flowcommerce/apidoc/service_level"
	"github.com/flowcommerce/apidoc/servicelevel"
)
""".trim)
  }

  it("handles keywords") {
    val builder = ImportBuilder(None)

    builder.ensureImport("io.flow.error.v0.models.error") should be("error_")
  }
  
}
