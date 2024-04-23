package generator.elm

case class ElmCommon(args: GenArgs) {

  def generate(): String = {
    args.imports.addAs("Json.Encode", "Encode")
    args.imports.addExposing("Http", "Header, Expect")
    """
      |encodeOptional : (a -> Encode.Value) -> Maybe a -> Encode.Value
      |encodeOptional encoder value =
      |    case value of
      |        Just v ->
      |            encoder v
      |
      |        Nothing ->
      |            Encode.null
      |
      |
      |type alias UnitResponse =
      |    {}
      |
      |type alias HttpRequestParams msg =
      |     { headers : List Header
      |       , expect : Expect msg
      |     }
      |""".stripMargin
  }

}
