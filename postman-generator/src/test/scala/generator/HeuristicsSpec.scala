package generator

import io.apibuilder.spec.v0.models.{Method, Operation, Resource}
import org.scalatest.WordSpec

class HeuristicsSpec extends WordSpec {

  "Postman Generator Heuristics" should {

    // Heuristics.idFieldHeuristic is not used anywhere, and I'm not sure this test is correct
    // both :organization and :key show up the same number of times, so how is it supposed to know which one is the id?
    "find an identifier param in a group of paths" ignore {
      val op1 = Operation(Method.Get, "/:organization/attributes/:key")
      val op2 = op1.copy(path = "/:organization/attributes/:key/path/some")
      val op3 = op2.copy(path = "/:organization/attributes/:key/path/some/other/path")

      val operations = Seq(op1, op2, op3)
      val resource = Resource("Resource", "some Resource", operations = operations)
      assert(Heuristics.idFieldHeuristic(resource).get == "key")

      assert(Heuristics.idFieldHeuristic(resource.copy(operations = operations.take(2))) == None)
    }
  }

}
