package generator

import com.gilt.apidoc.generator.v0.models.InvocationForm

trait CodeGenerator {

  def invoke(form: InvocationForm): String

}
