package generator.graphql

import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraphQLGeneratorSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
{
  def rightOrErrors[T](value: Either[_, T]): T = {
    value match {
      case Right(r) => r
      case Left(errors) => sys.error(s"Expected valid value but got: ${errors}")
    }
  }

  "Services with no types" in {
    val s = makeService()
    rightOrErrors(GraphQLGenerator.invoke(InvocationForm(s))).map(_.name) must equal(
      Seq("schema.graphql", "resolvers.ts", "type-metadata.ts")
    )
  }
}
