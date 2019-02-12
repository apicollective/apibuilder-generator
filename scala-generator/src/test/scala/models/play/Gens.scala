package scala.models.play

import io.apibuilder.spec.v0.models._
import io.apibuilder.spec.v0.models.gens._
import org.scalacheck.{Arbitrary, Gen}
import scala.generator.{ScalaEnum, ScalaModel, ScalaService, ScalaUnion}

package object gens {

  implicit lazy val arbitraryScalaService: Arbitrary[ScalaService] = Arbitrary(genScalaService)
  lazy val genScalaService: Gen[ScalaService] = for {
    service <- Arbitrary.arbitrary[Service]
    safeButBad = service.copy(
      models = service.models.map(_.copy(fields = Seq.empty)),
      unions = service.unions.map(_.copy(types = Seq.empty)),
      resources = service.resources.map { r =>
        val safe = r.operations.map(_.copy(
          body = None,
          parameters = Seq.empty,
          responses = Seq.empty,
        ))

        r.copy(operations = safe)
      }
    )
  } yield ScalaService(safeButBad)

  implicit lazy val arbitraryScalaEnum: Arbitrary[ScalaEnum] = Arbitrary(genScalaEnum)
  lazy val genScalaEnum: Gen[ScalaEnum] = for {
    service <- Arbitrary.arbitrary[ScalaService]
    enum <- Arbitrary.arbitrary[Enum]
  } yield new ScalaEnum(service, enum)

  implicit lazy val arbitraryScalaModel: Arbitrary[ScalaModel] = Arbitrary(genScalaModel)
  lazy val genScalaModel: Gen[ScalaModel] = for {
    service <- Arbitrary.arbitrary[ScalaService]
    model <- Arbitrary.arbitrary[Model]
    safeButBad = model.copy(fields = Seq.empty)
  } yield new ScalaModel(service, safeButBad)

  implicit lazy val arbitraryScalaUnion: Arbitrary[ScalaUnion] = Arbitrary(genScalaUnion)
  lazy val genScalaUnion: Gen[ScalaUnion] = for {
    service <- Arbitrary.arbitrary[ScalaService]
    union <- Arbitrary.arbitrary[Union]
    safeButBad = union.copy(types = Seq.empty)
  } yield new ScalaUnion(service, safeButBad)

}
