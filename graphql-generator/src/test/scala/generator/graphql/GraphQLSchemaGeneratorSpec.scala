package generator.graphql

import generator.graphql.helpers.TestHelpers
import io.apibuilder.builders.ApiBuilderServiceBuilders
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraphQLSchemaGeneratorSpec extends AnyWordSpec with Matchers
  with ApiBuilderServiceBuilders
  with TestHelpers
{

  "Services with no types" in {
    // TODO: Update apibuilder-validation before running these tests
    /*
    val s = makeService()
    rightOrErrors(GraphQLSchemaGenerator.invoke(InvocationForm(s))).map(_.name) must equal(
      Seq("schema.graphql")
    )
     */
  }
}
