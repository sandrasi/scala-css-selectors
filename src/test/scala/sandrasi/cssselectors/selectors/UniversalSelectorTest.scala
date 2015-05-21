package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class UniversalSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/universal_selector.xml"

  describe("construction") {

    it("constructs a universal selector with any namespace") {
      UniversalSelector() should be(UniversalSelector(AnyNamespace))
    }

    it("constructs a universal selector with no namespace") {
      UniversalSelector(null.asInstanceOf[String]) should be(UniversalSelector(NoNamespace))
    }

    it("constructs a universal selector with a namespace") {
      UniversalSelector("http://www.example.com") should be(UniversalSelector(UriNamespace("http://www.example.com")))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new UniversalSelector(null) }.getMessage should be("requirement failed: namespace is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = UniversalSelector()
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    it("selects all elements without a namespace") {
      UniversalSelector(NoNamespace).select(testXml) should be(trim(<root><ns:elem /><elem /></root><elem />))
    }

    it("selects all elements from the specified namespace") {
      UniversalSelector("http://www.example.com").select(testXml) should be(trim(<ns:elem />))
    }

    it("selects all elements from all namespaces") {
      UniversalSelector().select(testXml) should be(trim(<root><ns:elem /><elem /></root><ns:elem /><elem />))
    }
  }
}
