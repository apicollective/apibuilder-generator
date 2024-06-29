package generator.elm

import io.apibuilder.spec.v0.models.{Enum, EnumValue}

case class ElmEnum(args: GenArgs) {
  private val elmJson = ElmJson(args.imports)
  private val Unknown = "unknown"

  // "type MemberStatus = MemberStatusPending | MemberStatusActive | MemberStatusInactive | MemberStatusUnknown"
  def generate(e: Enum): String = {
    Seq(
      s"type ${Names.pascalCase(e.name)}\n    = " + values(e).mkString("\n    | "),
      genAll(e).code,
      genToString(e).code,
      genFromString(e).code,
      genEncoder(e).code,
      genDecoder(e).code
    ).mkString("\n\n")
  }

  private def values(e: Enum): Seq[String] = {
    (e.values.map(_.name) ++ Seq(Unknown)).map { name =>
      valueElmName(e, name)
    }
  }

  private def valueElmName(e: Enum, value: EnumValue): String = {
    valueElmName(e, value.name)
  }

  private def valueElmName(e: Enum, value: String): String = {
    Names.pascalCase(e.name + "_" + value)
  }


  /*
  memberStatusToString : MemberStatus -> String
  memberStatusToString typ =
      case typ of
          MemberStatusPending ->
              "pending"

          MemberStatusActive ->
              "active"

          MemberStatusInactive ->
              "inactive"

          MemberStatusUnknown ->
              "unknown"
   */
  private def genToString(e: Enum): ElmFunction = {
    def singleValue(name: String, value: String) = {
      Seq(
        s"${Names.maybeQuote(name)} ->",
        s"    ${Names.wrapInQuotes(value)}",
      ).mkString("\n").indent(8)
    }

    val n = s"${Names.camelCase(e.name)}ToString"
    val code = Seq(
      s"$n : ${Names.pascalCase(e.name)} -> String",
      s"$n instance =",
      "    case instance of",
      (e.values.map { v =>
        singleValue(
          name = valueElmName(e, v),
          value = v.value.getOrElse(v.name)
        )
      } ++ Seq(
        singleValue(
          name = valueElmName(e, Unknown),
          value = Unknown
        )
      )).mkString("\n")
    ).mkString("\n")
    ElmFunction(name = n, code = code)
  }

  /*
  * getAllValuesForNewsletterKey : List NewsletterKey
  * getAllValuesForNewsletterKey =
  *  [ NewsletterKeyGeneral, NewsletterKeyBilling ]
   */
  private def genAll(e: Enum): ElmFunction = {
    val n = s"getAll${Names.pascalCase(e.plural)}"

    val code = Seq(
      s"$n : List ${Names.pascalCase(e.name)}",
      s"$n =",
      s"    [ " + e.values.map { v => valueElmName(e, v) }.mkString(", ") + " ]"
    ).mkString("\n")
    ElmFunction(name = n, code = code)
  }

  /*
  memberStatusFromString : String -> MemberStatus
  memberStatusFromString value =
      if value == "active" then
          MemberStatusActive
      else if value == "pending" then
          MemberStatusPending
      else if value == "inactive" then
          MemberStatusInactive
      else
          MemberStatusUnknown
   */
  private def genFromString(e: Enum): ElmFunction = {
    def singleValue(isFirst: Boolean, name: String, value: String) = {
      val prefix = if (isFirst) { "" } else { "else " }
      Seq(
        s"${prefix}if value == ${Names.wrapInQuotes(value)} then",
        s"    $name"
      ).mkString("\n").indent(4)
    }

    val n = s"${Names.camelCase(e.name)}FromString"
    val code = Seq(
      s"$n : String -> ${Names.pascalCase(e.name)}",
      s"$n value =",
      e.values.zipWithIndex.flatMap { case (ev, i) =>
        (ev.value.toSeq ++ Seq(ev.name)).map { v =>
          singleValue(
            isFirst = i == 0,
            name = valueElmName(e, v),
            value = v
          )
        }
      }.mkString("\n"),
      "    else",
      "        " + valueElmName(e, Unknown)
    ).mkString("\n")
    ElmFunction(name = n, code = code)
  }

  /*
  memberStatusEncoder : MemberStatus -> Encode.Value
  memberStatusEncoder type_ =
      Encode.string (memberStatusToString type_)
   */
  private def genEncoder(e: Enum): ElmFunction = {
    elmJson.encoder(e.name) {
      s"Encode.string (${Names.camelCase(e.name)}ToString instance)"
    }
  }

  /*
  memberStatusDecoder : Decoder MemberStatus
  memberStatusDecoder =
      Decode.map memberStatusFromString string
   */
  private def genDecoder(e: Enum): ElmFunction = {
    elmJson.decoder(e.name) {
      s"Decode.map ${Names.camelCase(e.name)}FromString Decode.string"
    }
  }

}
