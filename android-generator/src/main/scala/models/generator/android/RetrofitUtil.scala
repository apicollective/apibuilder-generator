package models.generator.android

object RetrofitUtil {

  def toRetrofitPath(path: String): String = {

    //1. remove trailing slash, this is explained in https://github.com/square/retrofit/issues/1049

    val path1 = if(path.head == '/')
      path.tail
    else
      path

    //2. now replace :pathvars with {pathvars}

    val path2 = path1.replaceAll(":(.*?)/","{$1}/")

    //3. now do the same for when :pathvar is the last one in the string

    val path3 = path2.replaceAll(":(.*?)$","{$1}")

    path3
  }

}
