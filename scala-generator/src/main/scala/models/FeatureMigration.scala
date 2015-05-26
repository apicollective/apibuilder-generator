package scala.models

import lib.VersionTag
import com.gilt.apidoc.spec.v0.models.Service

case class FeatureMigration(serviceVersion: String) {

  private val version = VersionTag(serviceVersion)

  def hasImplicit404s(): Boolean = {
    versionLessThanOrEqual("0.9.4")
  }

  private def versionLessThanOrEqual(value: String): Boolean = {
    version.compare(VersionTag(value)) <= 0
  }

}
