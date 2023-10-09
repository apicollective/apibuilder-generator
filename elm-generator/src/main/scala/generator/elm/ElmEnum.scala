package generator.elm

import io.apibuilder.spec.v0.models.Enum

object ElmEnum {
  // "type MemberStatus = MemberStatusPending | MemberStatusActive | MemberStatusInactive | MemberStatusUnknown"
  def generate(e: Enum): String = {
    s"type ${Names.pascalCase(e.name)} = " + values(e).mkString(" | ")

  }

  private[this] def values(e: Enum): Seq[String] = {
    (e.values.map(_.name) ++ Seq("unknown")).map { name =>
      Names.pascalCase(e.name + "_" + name)
    }
  }
}
