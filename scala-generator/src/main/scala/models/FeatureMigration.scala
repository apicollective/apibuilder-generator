package scala.models

import io.apibuilder.spec.v0.models.Apidoc
import lib.VersionTag

object FeatureMigration {
  private val DefaultVersion: String = "0.9.5" // approximately when we deprecated apidoc field
  def apply(apidoc: Option[Apidoc]): FeatureMigration = {
    FeatureMigration(apidoc.map(_.version).getOrElse(DefaultVersion))
  }
}

case class FeatureMigration(serviceVersion: String) {

  private val version = VersionTag(serviceVersion)

  def hasImplicit404s(): Boolean = {
    versionLessThanOrEqual("0.9.4")
  }

  private def versionLessThanOrEqual(value: String): Boolean = {
    version.compare(VersionTag(value)) <= 0
  }

}
