package generator

import com.gilt.apidoc.generator.models.InvocationForm

trait CodeGenerator {

  def invoke(form: InvocationForm): String

}
