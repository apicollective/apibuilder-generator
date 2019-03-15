package generator

import generator.Heuristics.PathVariable
import io.flow.postman.v0.models._

object PredefinedCollectionItems {

  def testEventResponseStatusOk(testTitle: String): Event = {
    Event(
      EventType.Test,
      Some(Script(Seq(
        s"""pm.test("$testTitle", function () {""",
        """    pm.response.to.be.success;""",
        """});"""
      )))
    )
  }

  def testPostStatusOk(testTitle: String, pathVariable: Option[PathVariable]): Event = {
    Event(
      EventType.Test,
      Some(Script {
        val test = Seq(
          s"""pm.test("$testTitle", function () {""",
          """    pm.response.to.be.success;""",
          """});"""
        )

        val pathVariableSetup = pathVariable.map { pathVar =>
          Seq(
            """var jsonData = JSON.parse(responseBody);""",
            s"""var id = jsonData["${pathVar.name}"];""",
            s"""if (id != null) pm.environment.set("${pathVar.postmanVarName}", id);"""
          )
        }.getOrElse(Seq.empty)

        test ++ pathVariableSetup
      }
    ))
  }
}