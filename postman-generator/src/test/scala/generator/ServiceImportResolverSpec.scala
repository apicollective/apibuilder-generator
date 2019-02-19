package generator

import examples.ExampleJson
import io.apibuilder.spec.v0.models.Service
import org.scalatest.{Assertion, Matchers, WordSpec}

class ServiceImportResolverSpec extends WordSpec with Matchers {

  import TestFixtures._
  import scala.language.reflectiveCalls

  "ServiceImportResolver" should {

    "return the same service untouched if importedServices array is empty" in new TrivialServiceContext {
      val result = ServiceImportResolver.resolveService(trivialService, Seq.empty)

      result.service shouldEqual trivialService
    }

    "return a service with empty imports list" in new ResolvedServiceTestContext {
      result.service.imports.isEmpty shouldEqual true
    }

    "return a service, which enums, models and unions count equal the used services components sum" in new ResolvedServiceTestContext {
      resultService.enums.length shouldEqual mainService.enums.length + importedService.enums.length
      resultService.models.length shouldEqual mainService.models.length + importedService.models.length
      resultService.unions.length shouldEqual mainService.unions.length + importedService.unions.length
    }

    "group all resources into one map" in new ResolvedServiceTestContext {
      resultService.resources shouldEqual mainService.resources
      result.serviceNamespaceToResources shouldEqual Map(
        mainService.namespace -> mainService.resources,
        importedService.namespace -> importedService.resources
      )
    }

    "leave the enums, models, unions type intact if it comes from the main service" in new ResolvedServiceTestContext {
      def assertTypeNamesIntact[T](serviceToObjs: Service => Seq[T {def name: String}]): Assertion = {
        val sourceObjs = serviceToObjs(mainService)
        val resultObjs = serviceToObjs(resultService)
        val typeNamesIntact = sourceObjs.forall(obj => resultObjs.exists(_.name == obj.name))
        typeNamesIntact shouldEqual true
      }

      assertTypeNamesIntact(_.enums)
      assertTypeNamesIntact(_.models)
      assertTypeNamesIntact(_.unions)
    }

    "rename the merged enums, models, unions type to the full namespace+name" in new ResolvedServiceTestContext {
      def assertTypeNamesWithFullNamespace[T](serviceToObjs: Service => Seq[T {def name: String}]): Assertion = {
        val sourceObjs = serviceToObjs(importedService)
        val resultObjs = serviceToObjs(resultService)
        val typeNamesWithFullNamespace = sourceObjs.forall { sourceEnum =>
          resultObjs.exists(resultEnum =>
            resultEnum.name.contains(importedService.namespace) &&
              resultEnum.name.contains(sourceEnum.name)
          )
        }
        typeNamesWithFullNamespace shouldEqual true
      }

      assertTypeNamesWithFullNamespace(_.enums)
      assertTypeNamesWithFullNamespace(_.models)
      assertTypeNamesWithFullNamespace(_.unions)
    }

    "rename complex field types in the imported models so they match the full namespace+type" in new ResolvedServiceTestContext {
      val fieldNameToTypeMap = importedService.models.flatMap(_.fields).map(f => (f.name, f.`type`)).toMap
      val fieldNamesToCheck = fieldNameToTypeMap.filter {
        case (_, typ) => importedService.models.map(_.name).exists(n => typ.contains(n))
      }.keys.toList

      val updatedFieldsToTest = resultService.models.flatMap(_.fields)
        .filter(f => fieldNamesToCheck.contains(f.name))

      updatedFieldsToTest.foreach { field =>
        field.`type` should include(importedService.namespace)
      }
    }

    "prepare models in a way, which enables ExampleJson to prepare a sample JSON for every single one of them" in new ResolvedServiceTestContext {
      val exampleProvider = ExampleJson.allFields(resultService)
      resultService.models.foreach { model =>
        val exampleProvided = exampleProvider.sample(model.name).isDefined
        assert(exampleProvided, s"// it should provide an example for ${model.name}")
      }
    }

    "rename complex types in the imported unions so they match the full namespace+type" in new ResolvedServiceWithUnionsTestContext {
      val unionNameToTypesMap = importedService.unions.map(u => (u.name, u.types)).toMap
      val typesDefinedInService = importedService.models.map(_.name) ++ importedService.enums.map(_.name)
      val unionNameToTypesMapToCheck = unionNameToTypesMap.map {
        case (name, types) =>
          val filteredTypes = types.filter(typ => typesDefinedInService.exists(n => typ.`type`.contains(n)))
          (name, filteredTypes)
      }
      val updatedUnionTypesToTest = resultService.unions
        .filter(u => unionNameToTypesMapToCheck.keys.exists(k => u.name.contains(k)))
        .flatMap { union =>
          union.types.filter { typ =>
            unionNameToTypesMapToCheck.values.flatten.exists(u => typ.`type`.contains(u.`type`))
          }
        }

      updatedUnionTypesToTest.foreach { typ =>
        typ.`type` should include(importedService.namespace)
      }
    }

    "prepare unions in a way, which enables ExampleJson to prepare a sample JSON for every single one of them" in new ResolvedServiceWithUnionsTestContext {
      val exampleProvider = ExampleJson.allFields(resultService)
      resultService.unions.foreach { union =>
        val exampleProvided = exampleProvider.sample(union.name).isDefined
        assert(exampleProvided, s"// it should provide an example for ${union.name}")
      }
    }

    "import multiple services" in new ResolvedServiceWithTwoImportsTestContext {
      resultService.resources shouldEqual mainService.resources
      result.serviceNamespaceToResources shouldEqual Map(
        mainService.namespace -> mainService.resources,
        importedService1.namespace -> importedService1.resources,
        importedService2.namespace -> importedService2.resources
      )
    }
  }

  trait ResolvedServiceTestContext extends TrivialServiceWithImportCtx {
    val mainService = trivialServiceWithImport
    val importedService = models.TestHelper.referenceApiService

    lazy val result = ServiceImportResolver.resolveService(mainService, Seq(importedService))
    lazy val resultService = result.service
  }

  trait ResolvedServiceWithUnionsTestContext extends TrivialServiceWithUnionTypesImportCtx {
    val mainService = trivialServiceWithUnionTypesImport
    val importedService = models.TestHelper.generatorApiServiceWithUnionWithoutDescriminator

    lazy val result = ServiceImportResolver.resolveService(mainService, Seq(importedService))
    lazy val resultService = result.service
  }

  trait ResolvedServiceWithTwoImportsTestContext extends TrivialServiceWithTwoImportsCtx {
    val mainService = trivialServiceWithTwoImports
    val importedService1 = models.TestHelper.referenceApiService
    val importedService2 = models.TestHelper.generatorApiServiceWithUnionWithoutDescriminator

    lazy val result = ServiceImportResolver.resolveService(mainService, Seq(importedService1, importedService2))
    lazy val resultService = result.service
  }

}
