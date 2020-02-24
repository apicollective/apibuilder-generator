package scala.models

import lib.VersionTag

case class FeatureMigration(serviceVersion: String) {

  private[this] val version = VersionTag(serviceVersion)

  def hasImplicit404s(): Boolean = {
    versionLessThanOrEqual("0.9.4")
  }

  private def versionLessThanOrEqual(value: String): Boolean = {
    version.compare(VersionTag(value)) <= 0
  }

}
