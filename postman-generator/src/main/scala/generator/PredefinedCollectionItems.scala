package generator

import generator.Heuristics.PathVariable
import io.flow.postman.collection.v210.v0.models._

object PredefinedCollectionItems {

  import Utils._

  val requestUrl = Url(
    raw = Some(s"{{BASE_URL}}/organizations"),
    protocol = None,
    host = Some(Seq("{{BASE_URL}}")),
    path = Some(Seq("organizations"))
  )

  def prepareSetupFolder(): Folder = {
    val step1 = Item(
      name = Some("Step 1 - Select parent organization"),
      description = Some(Description(
        """Fetch all organizations and select first production organization for a parent.
          |Also selects randomized organization name and sets a postman variable.""".stripMargin)),
      request = Request(
        url = Some(requestUrl),
        method = Some(Method.Get)
      ),
      event = Some(Seq(Event(
          EventType.Test,
          Some(Script(Seq(
            """var jsonData = JSON.parse(responseBody);""",
            """var prodOrg = jsonData.filter(org => org.environment == "production");""",
            """postman.setEnvironmentVariable("organization-parent", prodOrg[0].id);""",
            """var randomId = Math.random().toString(36).substring(7);""",
            """pm.environment.set("ORGANIZATION", prodOrg[0].id + "-demo-" + randomId)"""
          ))))
        )
      )
    )

    val step2 = Item(
      name = Some("Step 2 - Create organization with randomized name"),
      description = None,
      request = Request(
        url = Some(requestUrl),
        method = Some(Method.Post),
        body = Some(Body(Some {
          """{
            |  "parent_id" : "{{organization-parent}}",
            |  "environment": "sandbox",
            |  "name": "{{ORGANIZATION}}"
            |}
          """.stripMargin
        }
        )),
        header = Some(Seq(Header("Content-Type", "application/json")))
      )
    )

    Folder(
      name = "Setup",
      description = None,
      item = Seq(step1, step2)
    )
  }

  def prepareCleanupFolder(): Folder = {
    val cleanup = Item(
      name = Some("Delete demo organization"),
      description = None,
      request = Request(
        url = Some(requestUrl.copy(path = requestUrl.path.map(_.:+("{{ORGANIZATION}}")))),
        method = Some(Method.Delete),
        header = Some(Seq(Header("Content-Type", "application/json")))
      )
    )

    Folder(
      name = "Cleanup",
      description = None,
      item = Seq(cleanup)
    )
  }

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