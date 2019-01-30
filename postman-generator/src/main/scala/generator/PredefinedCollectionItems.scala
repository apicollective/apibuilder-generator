package generator

import models.postman._
import generator.Heuristics.PathVariable

object PredefinedCollectionItems {

  val requestUrl = PostmanRequestUrl(
    raw = s"{{BASE_URL}}/organizations",
    protocol = "",
    host = Seq("{{BASE_URL}}"),
    path = Seq("organizations"))

  def prepareSetupFolder(): PostmanCollectionFolder = {
    val step1 = PostmanCollectionItem(
      id = None,
      name = Some("Step 1 - Get available organizations"),
      description = None,
      request = PostmanRequest(
        url = requestUrl,
        method = "GET"
      ),
      event = Seq(
        Event(
          EventType.test,
          Script(Seq(
            """var jsonData = JSON.parse(responseBody);""",
            """var prodOrg = jsonData.filter(org => org.environment == "production");""",
            """postman.setEnvironmentVariable("organization-parent", prodOrg[0].id);""",
            """var randomId = Math.random().toString(36).substring(7);""",
            """pm.environment.set("ORGANIZATION", prodOrg[0].id + "-demo-" + randomId)"""
          ))
        )
      )
    )

    val step2 = PostmanCollectionItem(
      id = None,
      name = Some("Step 2 - Create organization with randomized name"),
      description = None,
      request = PostmanRequest(
        url = requestUrl,
        method = "POST",
        body = Some(PostmanRequestBodyRaw(
          """{
            |  "parent_id" : "{{organization-parent}}",
            |  "environment": "sandbox",
            |  "name": "{{ORGANIZATION}}"
            |}
          """.stripMargin
        )),
        headers = Seq(PostmanHeader("Content-Type", "application/json"))
      )
    )

    PostmanCollectionFolder(
      name = "Setup",
      description = None,
      item = Seq(step1, step2)
    )
  }

  def prepareCleanupFolder(): PostmanCollectionFolder = {
    val cleanup = PostmanCollectionItem(
      id = None,
      name = Some("Delete demo organization"),
      description = None,
      request = PostmanRequest(
        requestUrl.copy(path = requestUrl.path.:+("{{ORGANIZATION}}")),
        method = "DELETE",
        headers = Seq(PostmanHeader("Content-Type", "application/json"))
      )
    )

    PostmanCollectionFolder(
      name = "Cleanup",
      description = None,
      item = Seq(cleanup)
    )
  }

  def testEventResponseStatusOk(testTitle: String): Event = {
    Event(
      EventType.test,
      Script(Seq(
        s"""pm.test("$testTitle", function () {""",
        """    pm.response.to.be.success;""",
        """});"""
      ))
    )
  }

  def testPostStatusOk(testTitle: String, pathVariable: Option[PathVariable]): Event = {
    Event(
      EventType.test,
      Script{
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
    )
  }
}