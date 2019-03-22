package examples

import scala.util.Random
import scala.util.hashing.MurmurHash3

trait RandomStringGenerator {
  def generate(seed: String): String
}

object ScalaRandomStringGenerator extends RandomStringGenerator {

  override def generate(seed: String): String = {
    val string = randomStringStream.take(6).mkString
    s"lorem_ipsum_$string"
  }

  private def randomStringStream: Stream[Char] =
    Random.alphanumeric.dropWhile(_.isDigit)
}

object MurmurRandomStringGenerator extends RandomStringGenerator {
  override def generate(seed: String): String = {
    val hash = MurmurHash3.stringHash(seed).toString.takeRight(6)
    s"lorem_ipsum_$hash"
  }
}
