package generator

import com.gilt.apidocgenerator.models.Service

trait CodeGenerator {
  def generate(sd: Service): String
}
