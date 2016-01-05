package models.generator

import com.bryzek.apidoc.spec.v0.models.{Field, Model, UnionType, Union, Info, Apidoc, Organization, Application, EnumValue, Enum, Service}
import org.scalatest.{ShouldMatchers, FunSpec}
import org.scalatest.mock.MockitoSugar

import models.generator.JavaClasses.Generator

/**
 *
 * Author: jkenny
 * Date: 17/07/2015
 */
class JavaClassesSpec extends FunSpec with ShouldMatchers with MockitoSugar {

  val testHeader = """/** Test Header */"""
  val userDefinedModel = Model("user_defined", "", None, None, Seq.empty[Field])
  val testService = Service(mock[Apidoc], "test_service", mock[Organization], mock[Application], "com.jkenny.test", "", info = mock[Info], models = Seq(userDefinedModel))
  val generator = new Generator(testService, Some(testHeader))

  describe("generateEnum") {
    it("should generate the correct source") {
      val source = generator.generateEnum(Enum("test_enum", "", None, None, Seq(EnumValue("value_1"), EnumValue("value_2"))))

      source.name shouldBe "TestEnum.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |public enum TestEnum {
          |    VALUE_1,
          |    VALUE_2
          |}""".stripMargin
    }

    it("should handle enums with descriptions") {
      val source = generator.generateEnum(Enum("test_enum", "", Some("A nice description"), None, Seq(EnumValue("value_1", Some("Nice description for 1")), EnumValue("value_2", Some("Nice description for 2")))))

      source.name shouldBe "TestEnum.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |/**
          | * A nice description
          | */
          |public enum TestEnum {
          |    /**
          |     * Nice description for 1
          |     */
          |    VALUE_1,
          |    /**
          |     * Nice description for 2
          |     */
          |    VALUE_2
          |}""".stripMargin
    }
  }

  describe("generateUnionType") {
    it("should generate the correct source") {
      val source = generator.generateUnionType(Union("test_union", "", None, None, None, Seq.empty[UnionType]))

      source.name shouldBe "TestUnion.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |public interface TestUnion {}""".stripMargin
    }

    it("should handle unions with descriptions") {
      val source = generator.generateUnionType(Union("test_union", "", None, Some("A nice description"), None, Seq.empty[UnionType]))

      source.name shouldBe "TestUnion.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |/**
          | * A nice description
          | */
          |public interface TestUnion {}""".stripMargin
    }
  }

  describe("generateUndefinedUnionType") {
    it("should generate the correct source") {
      val source = generator.generateUndefinedUnionType(Union("test_union", "", None, None, None, Seq.empty[UnionType]))

      source.name shouldBe "TestUnionUndefinedType.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |/**
          | * Provides future compatibility in clients - in the future, when a type is added to the union TestUnion, it will need to be handled in the client code. This implementation will deserialize these future types as an instance of this class.
          | */
          |public class TestUnionUndefinedType implements TestUnion {
          |    /**
          |     * Information about the type that we received that is undefined in this version of the client.
          |     */
          |    private String description;
          |
          |    public TestUnionUndefinedType() {}
          |}""".stripMargin
    }
  }

  describe("generateNativeWrapper") {
    it("should generate the correct source") {
      val source = generator.generateNativeWrapper(Union("test_union", "", None, None, None, Seq.empty[UnionType]), JavaDatatypes.Boolean)

      source.name shouldBe "TestUnionBoolean.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |/**
          | * Wrapper class to support the datatype 'boolean' in the union TestUnion.
          | */
          |public class TestUnionBoolean implements TestUnion {
          |    private boolean value;
          |
          |    public TestUnionBoolean() {}
          |}""".stripMargin
    }
  }

  describe("generateModel") {
    it("should generate the correct source") {
      val source = generator.generateModel(Model("test_model", "", None, None, Seq(
        Field("required_field_with_default", "boolean", None, None, Some("false"), true),
        Field("required_field", "boolean", None, None, None, true),
        Field("optional_field", "boolean", None, None, None, false),
        Field("user_defined_field", "user_defined", None, None, None, true)
      )), Seq.empty[Union])

      source.name shouldBe "TestModel.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |public class TestModel {
          |    private boolean requiredFieldWithDefault = false;
          |
          |    private boolean requiredField;
          |
          |    private boolean optionalField;
          |
          |    private UserDefined userDefinedField;
          |
          |    public TestModel() {}
          |}""".stripMargin
    }

    it("should handle models with descriptions") {
      val source = generator.generateModel(Model("test_model", "", Some("A nice description"), None, Seq(
        Field("required_field_with_default", "boolean", Some("Nice description for required_field_with_default"), None, Some("false"), true),
        Field("required_field", "boolean", Some("Nice description for required_field"), None, None, true),
        Field("optional_field", "boolean", Some("Nice description for optional_field"), None, None, false)
      )), Seq.empty[Union])

      source.name shouldBe "TestModel.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |/**
          | * A nice description
          | */
          |public class TestModel {
          |    /**
          |     * Nice description for required_field_with_default
          |     */
          |    private boolean requiredFieldWithDefault = false;
          |
          |    /**
          |     * Nice description for required_field
          |     */
          |    private boolean requiredField;
          |
          |    /**
          |     * Nice description for optional_field
          |     */
          |    private boolean optionalField;
          |
          |    public TestModel() {}
          |}""".stripMargin
    }

    it("should handle models that are part of union types") {
      val source = generator.generateModel(Model("test_model", "", None, None, Seq(
        Field("required_field_with_default", "boolean", None, None, Some("false"), true),
        Field("required_field", "boolean", None, None, None, true),
        Field("optional_field", "boolean", None, None, None, false)
      )), Seq(Union("union_one", "", None, None, None, Seq.empty[UnionType]), Union("union_two", "", None, None, None, Seq.empty[UnionType])))

      source.name shouldBe "TestModel.java"
      source.dir shouldBe Some("com/jkenny/test/models")
      source.contents shouldBe
        """/** Test Header */
          |package com.jkenny.test.models;
          |
          |public class TestModel implements UnionOne, UnionTwo {
          |    private boolean requiredFieldWithDefault = false;
          |
          |    private boolean requiredField;
          |
          |    private boolean optionalField;
          |
          |    public TestModel() {}
          |}""".stripMargin
    }
  }
}
