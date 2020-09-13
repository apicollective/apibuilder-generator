package generator.graphql

import generator.graphql.helpers.TestHelpers
import io.apibuilder.builders.ApiBuilderServiceBuilders
import io.apibuilder.generator.v0.models.InvocationForm
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraphQLSchemaGeneratorSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with TestHelpers
{

  "Services with no types" in {
    val s = makeService()
    rightOrErrors(GraphQLSchemaGenerator.invoke(InvocationForm(s))).map(_.name) must equal(
      Seq("schema.graphql")
    )
  }
}
