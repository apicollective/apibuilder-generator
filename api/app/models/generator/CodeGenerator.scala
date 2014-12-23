package generator

import com.gilt.apidocgenerator.models.InvocationForm

trait CodeGenerator {

  def invoke(form: InvocationForm): String

}
