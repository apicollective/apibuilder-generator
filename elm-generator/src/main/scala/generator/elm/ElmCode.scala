package generator.elm

import scala.collection.concurrent.TrieMap

sealed trait ElmCode {
  def name: String
  def code: String
}

sealed trait ElmVariable {
  def name: String
  def typeName: String
}

case class ElmTypeAlias(name: String, typeName: String, code: String) extends ElmCode with ElmVariable
case class ElmFunction(name: String, code: String) extends ElmCode
case class ElmParameter(name: String, typeName: String) extends ElmVariable


case class ElmTypeAliasBuilder(
                              variableName: String,
                                typeName: String,
                                properties: Seq[(String, ElmType)] = Nil
                              ) {
  assert(Names.pascalCase(typeName) == typeName, s"Name must be in pascal case")

  def addProperty(name: String, typ: ElmType): ElmTypeAliasBuilder = {
    this.copy(
      properties = properties ++ Seq((Names.camelCase(name), typ))
    )
  }

  private def singleRequiredProperty: Option[ElmParameter] = {
    properties.toList match {
      case (name, typ) :: Nil => {
        import ElmType.*
        typ match {
          case ElmString | ElmInt | ElmBool | ElmDate | ElmFloat | ElmPosix | ElmDict(_) | ElmList(_) | ElmEnumLocal(_) | _: ElmEnumImported | ElmUserDefinedLocal(_) | _: ElmUserDefinedImported => Some(
            ElmParameter(name, typ.declaration)
          )
          case ElmNothing | ElmMaybe(_) => None
        }
      }
      case _ => None
    }
  }

  def build(): Option[ElmVariable] = {
    if (properties.isEmpty) {
      None

    } else {
      singleRequiredProperty.orElse {
        val code = s"type alias $typeName =\n" + properties.map { case (k,v) => s"$k : ${v.declaration}"}.mkString("    {", "\n    , ", "\n    }")
        Some(ElmTypeAlias(variableName, typeName, code))
      }
    }
  }
}

case class ElmFunctionBuilder(
                                name: String,
                                parameters: Seq[(String, String)] = Nil,
                                bodies: Seq[String] = Nil,
                                returnType: Option[String] = None,
                              ) {
  assert(Names.camelCase(name) == name, s"Name must be in camel case")

  def addParameter(name: String, typ: Option[String]): ElmFunctionBuilder = {
    typ match {
      case None => this
      case Some(t) => this.copy(
        parameters = parameters ++ Seq((name, t))
      )
    }
  }

  def addParameter(name: String, typ: String): ElmFunctionBuilder = {
    addParameter(name, Some(typ))
  }

  def addBody(body: String): ElmFunctionBuilder = {
    this.copy(
      bodies = bodies ++ Seq(body)
    )
  }

  def addReturnType(returnType: String): ElmFunctionBuilder = {
    this.copy(
      returnType = Some(returnType)
    )
  }

  def build(): ElmFunction = {
    val body = Seq(
      argList(parameters.map(_._2)) + s" -> " + returnType.getOrElse { sys.error("Missing return type") },
      name + " " + parameters.map(_._1).mkString(" ") + " =",
      bodies.mkString("\n").strip.indent(4)
    ).mkString("\n")
    ElmFunction(name, body)
  }

  private def argList(all: Seq[String]): String = {
    name + (all.toList match {
      case Nil => ""
      case one :: Nil => s" : $one"
      case one :: rest => s" : $one -> " + rest.mkString(" -> ")
    })
  }
}


case class ElmFunctions() {
  private val all = TrieMap[String, Unit]()

  def add(body: String): Unit = {
    all.put(body.strip, ())
  }

  def generateCode(): String = {
    all.keysIterator.toSeq.sorted.mkString("\n\n")
  }
}
