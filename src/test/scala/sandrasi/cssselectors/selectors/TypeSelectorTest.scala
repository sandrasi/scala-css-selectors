package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class TypeSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/type_selector.xml"

  describe("construction") {

    it("constructs a type selector with an element name in any namespace") {
      TypeSelector("elem") should be(TypeSelector(AnyNamespace, "elem"))
    }

    it("constructs a type selector with an element name in no namespace") {
      TypeSelector(null.asInstanceOf[String], "elem") should be(TypeSelector(NoNamespace, "elem"))
    }

    it("constructs a type selector with an element name in a namespace") {
      TypeSelector("http://www.example.com", "elem") should be(TypeSelector(UriNamespace("http://www.example.com"), "elem"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new TypeSelector(null, "elem") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires a type name") {
      intercept[IllegalArgumentException] { new TypeSelector(NoNamespace, null) }.getMessage should be("requirement failed: name is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = TypeSelector("elem")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all the specified elements without a namespace") {
        TypeSelector(NoNamespace, "elem").select(testXml) should be(trim(<elem />))
      }

      it("selects all the specified elements from the specified namespace") {
        TypeSelector("http://www.example.com", "elem").select(testXml) should be(trim(<ns:elem />))
      }

      it("selects all the specified elements from all namespaces") {
        TypeSelector("elem").select(testXml) should be(trim(<ns:elem /><elem />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveElementNames

      it("selects all the specified elements without a namespace") {
        TypeSelector(NoNamespace, "elem").select(testXml) should be(trim(<elem /><ELEM />))
      }

      it("selects all the specified elements from the specified namespace") {
        TypeSelector("http://www.example.com", "elem").select(testXml) should be(trim(<ns:elem /><ns:ELEM />))
      }

      it("selects all the specified elements from all namespaces") {
        TypeSelector("elem").select(testXml) should be(trim(<ns:elem /><ns:ELEM /><elem /><ELEM />))
      }
    }
  }
}
