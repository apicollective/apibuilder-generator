package examples

import java.util.UUID

import scala.util.hashing.MurmurHash3

trait RandomStringGenerator {
  def generate(seed: String): String
}

object UuidRandomStringGenerator extends RandomStringGenerator {

  override def generate(seed: String): String = {
    UUID.randomUUID().toString.toLowerCase.takeRight(12)
  }

}

object MurmurRandomStringGenerator extends RandomStringGenerator {
  override def generate(seed: String): String = {
    val hash = MurmurHash3.stringHash(seed).toString.takeRight(6)
    s"lorem_ipsum_$hash"
  }
}
