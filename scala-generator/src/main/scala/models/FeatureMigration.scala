package scala.models

import lib.generator.VersionTag
import com.bryzek.apidoc.spec.v0.models.Service

case class FeatureMigration(serviceVersion: String) {

  private[this] val version = VersionTag(serviceVersion)

  def hasImplicit404s(): Boolean = {
    versionLessThanOrEqual("0.9.4")
  }

  private def versionLessThanOrEqual(value: String): Boolean = {
    version.compare(VersionTag(value)) <= 0
  }

}
