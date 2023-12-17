package generator.elm

case class ElmCommon(args: GenArgs) {

  def generate(): String = {
    args.imports.addAs("Json.Encode", "Encode")
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
      |""".stripMargin
  }

}
