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
      |boolToString : Bool -> String
      |boolToString value =
      |    if value then
      |        "true"
      |
      |    else
      |        "false"
      |
      |type alias UnitResponse =
      |    {}
      |
      |type alias HttpRequestParams msg =
      |     { apiHost: String
      |       , communityId: String
      |       , headers : List Header
      |       , expect : Expect msg
      |     }
      |""".stripMargin
  }

}
