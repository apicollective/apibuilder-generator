package models.generator.android

import com.squareup.javapoet.ClassName
import retrofit2.Call

object AndroidClasses
  extends BaseAndroidCodeGenerator {
  override def getJavaDocFileHeader() = "WARNING: not all features (notably unions) and data types work with android generator yet.  \n" +
    "Android generator is designed to be used in an android application, but should work in any java codebase as long as you import jackson and retrofit2 libraries.  \n" +
    "If you are considering using this library, would like to request/discuss features, or would like to share how you're using it, please contact android-feedback@gilt.com \n"

  override def getRetrofitReturnTypeWrapperClass() = ClassName.get(classOf[Call[Void]])
}
