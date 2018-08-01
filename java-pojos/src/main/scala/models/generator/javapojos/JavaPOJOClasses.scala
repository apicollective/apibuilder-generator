package models.generator.javapojos

object JavaPOJOClasses
  extends BaseJavaPOJOCodeGenerator {
  override def getJavaDocFileHeader() = "WARNING: not all features (notably unions) and data types work with the java generator yet.  \n" +
    "Java POJO generator is designed to be used with AWS Lambdas.  \n" +
    "If you are considering using this library, would like to request/discuss features, or would like to share how you're using it, please contact daniel.kirby@gilt.com \n"

}
