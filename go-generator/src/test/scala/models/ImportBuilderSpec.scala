package go.models

import org.scalatest.{FunSpec, Matchers}

class ImportBuilderSpec extends FunSpec with Matchers {

  it("empty if no imports") {
    val builder = ImportBuilder()
    builder.generate() should be("")
  }

  it("prefixes with package name") {
    val builder = ImportBuilder()
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
    val builder = ImportBuilder()
    builder.ensureImport("io")
    builder.ensureImport("io")
    builder.generate() should be("""
import (
	"io"
)
""".trim)
  }

  it("alphabetizes") {
    val builder = ImportBuilder()
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
    val builder = ImportBuilder()
    builder.ensureImport("io.flow.common.v0.models")
    builder.generate() should be("""
import (
	common "github.com/flowcommerce/common"
)
""".trim)
  }

  it("aliases duplicate imports") {
    val builder = ImportBuilder()
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
    val builder = ImportBuilder()
    builder.ensureImport("common")
    builder.ensureImport("io.flow.common.v0.models")
    builder.generate() should be("""
import (
	"common"
	flowcommerceCommon "github.com/flowcommerce/common"
)
""".trim)
  }

}
