package generator.csharp

case class RecordField(name: String, `type`: String, required: Boolean)

case class RecordBuilder(
  name: Option[String] = None,
  fields: Seq[RecordField] = Nil
) {
  def withName(name: String): RecordBuilder = {
    this.copy(
      name = Some(name)
    )
  }

  def withField(field: RecordField): RecordBuilder = {
    this.copy(
      fields = fields ++ Seq(field)
    )
  }

  def build: String = {
    val n = name.getOrElse {
      sys.error("Missing class name")
    }
    Seq(
      s"public record $n (",
      fields.map { f =>
        val opt = if (f.required) { "" } else { "?" }
        s"${f.`type`}$opt ${f.name}"
      }.mkString(",\n").trim.indent(2).stripTrailing(),
      ");",
    ).mkString("\n")
  }

}
