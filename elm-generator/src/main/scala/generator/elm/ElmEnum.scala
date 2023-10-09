package generator.elm

import io.apibuilder.spec.v0.models.{Enum, EnumValue}

case class ElmEnum(args: GenArgs) {
  private[this] val elmJson = ElmJson(args.imports)
  private[this] val Unknown = "unknown"

  // "type MemberStatus = MemberStatusPending | MemberStatusActive | MemberStatusInactive | MemberStatusUnknown"
  def generate(e: Enum): String = {
    Seq(
      s"type ${Names.pascalCase(e.name)} = " + values(e).mkString(" | "),
      genToString(e),
      genFromString(e),
      genEncoder(e),
      genDecoder(e)
    ).mkString("\n\n")
  }

  private[this] def values(e: Enum): Seq[String] = {
    (e.values.map(_.name) ++ Seq(Unknown)).map { name =>
      valueElmName(e, name)
    }
  }

  private[this] def valueElmName(e: Enum, value: EnumValue): String = {
    valueElmName(e, value.name)
  }

  private[this] def valueElmName(e: Enum, value: String): String = {
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
  private[this] def genToString(e: Enum): String = {
    def singleValue(name: String, value: String) = {
      Seq(
        s"${Names.maybeQuote(name)} ->",
        s"    ${Names.wrapInQuotes(value)}",
      ).mkString("\n").indent(8)
    }
    Seq(
      s"${Names.camelCase(e.name)}ToString : ${Names.pascalCase(e.name)} -> String",
      s"${Names.camelCase(e.name)}ToString instance =",
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
  }

  /*
  memberStatusFromString : String -> MemberStatus
  memberStatusFromString value =
      if (value == "active") then
          MemberStatusActive
      else if (value == "pending") then
          MemberStatusPending
      else if (value == "inactive") then
          MemberStatusInactive
      else
          MemberStatusUnknown
   */
  private[this] def genFromString(e: Enum): String = {
    def singleValue(isFirst: Boolean, name: String, value: String) = {
      val prefix = if (isFirst) { "" } else { "else " }
      Seq(
        s"${prefix}if (value == ${Names.wrapInQuotes(value)}) then",
        s"    $name"
      ).mkString("\n").indent(4)
    }

    Seq(
      s"${Names.camelCase(e.name)}FromString : String -> ${Names.pascalCase(e.name)}",
      s"${Names.camelCase(e.name)}FromString value =",
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
  }

  /*
  memberStatusEncoder : MemberStatus -> Encode.Value
  memberStatusEncoder type_ =
      Encode.string (memberStatusToString type_)
   */
  private[this] def genEncoder(e: Enum): String = {
    elmJson.encoder(e.name) {
      s"Encode.string (${Names.camelCase(e.name)}ToString instance)"
    }
  }

  /*
  memberStatusDecoder : Decoder MemberStatus
  memberStatusDecoder =
      Decode.map memberStatusFromString string
   */
  private[this] def genDecoder(e: Enum): String = {
    elmJson.decoder(e.name) {
      s"Decode.map ${Names.camelCase(e.name)}FromString Decode.string"
    }
  }

}
