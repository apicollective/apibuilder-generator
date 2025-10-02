package scala.generator

case class ScalaEnums(
  ssd: ScalaService,
  enumDef: ScalaEnum
) {

  private val unions = ssd.unionsForEnum(enum)

  def build(): String = {
    import lib.Text.*
    Seq(
      enumDef.description.map { desc => ScalaUtil.textToComment(desc) + "\n" }.getOrElse("") +
      s"sealed trait ${enumDef.name}" + ScalaUtil.extendsClause(
        className = enumDef.name,
        interfaces = Nil,
        unions = unions.map(_.name),
      ).getOrElse(" extends _root_.scala.Product with _root_.scala.Serializable"),
      s"${ScalaUtil.deprecationString(enumDef.deprecation)}object ${enumDef.name} {",
      buildValues().indentString(2),
      s"}"
    ).mkString("\n\n")
  }
  
  private def buildValues(): String = {
    (enumDef.values.map { value =>
      CaseClassBuilder()
        .withName(value.name)
        .withExtendsClasses(Seq(enumDef.name))
        .withDeprecation(value.deprecation)
        .withScaladoc(value.description.map(ScalaUtil.textToComment))
        .withBodyParts(Seq(
          s"override def toString = ${ScalaUtil.wrapInQuotes(value.serializedValue)}"
        ))
        .withBodyParts(DiscriminatorValue.generateCode(enum, unions))
        .build
    } ++ Seq(
      CaseClassBuilder()
        .withName("UNDEFINED")
        .withExtendsClasses(Seq(enumDef.name))
        .withScaladoc(
          Some("\n" + """
           |/**
           | * UNDEFINED captures values that are sent either in error or
           | * that were added by the server after this library was
           | * generated. We want to make it easy and obvious to users of
           | * this library to handle this case gracefully.
           | *
           | * We use all CAPS for the variable name to avoid collisions
           | * with the camel cased values above.
           | */
           |""".stripMargin.strip
          )
        )
        .withArgList(Some("override val toString: String"))
        .withBodyParts(DiscriminatorValue.generateCode(enum, unions))
        .build
    )).mkString("\n") + """

/**
 * all returns a list of all the valid, known values. We use
 * lower case to avoid collisions with the camel cased values
 * above.
 */
    """.stripTrailing() + "\n" +
    s"val all: scala.List[${enumDef.name}] = scala.List(" + enumDef.values.map(_.name).mkString(", ") + ")\n\n" +
    s"private\n" +
    s"val byName: Map[String, ${enumDef.name}] = all.map(x => x.toString.toLowerCase -> x).toMap\n\n" +
    s"def apply(value: String): ${enumDef.name} = fromString(value).getOrElse(UNDEFINED(value))\n\n" +
    s"def fromString(value: String): _root_.scala.Option[${enumDef.name}] = byName.get(value.toLowerCase)\n\n"
  }

}
