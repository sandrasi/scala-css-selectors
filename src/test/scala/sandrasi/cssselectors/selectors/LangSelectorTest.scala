package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration
import sandrasi.cssselectors.util.{AnyNamespace, NoNamespace, UriNamespace}

class LangSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/lang_selector.xml"

  describe("construction") {

    it("constructs a language selector with the lang attribute name and a value in any namespace") {
      LangSelector("en") should be(LangSelector(AnyNamespace, "lang", "en"))
    }

    it("constructs a language selector with the lang attribute name and a value in no namespace") {
      LangSelector(null, "en") should be(LangSelector(NoNamespace, "lang", "en"))
    }

    it("constructs a language selector with the lang attribute name and a value in a namespace") {
      LangSelector("http://www.example.com", "en") should be(LangSelector(UriNamespace("http://www.example.com"), "lang", "en"))
    }

    it("constructs a language selector with an attribute name and a value in no namespace") {
      LangSelector(null.asInstanceOf[String], "lang", "en") should be(LangSelector(NoNamespace, "lang", "en"))
    }

    it("constructs a language selector with an attribute name and a value in a namespace") {
      LangSelector("http://www.example.com", "lang", "en") should be(LangSelector(UriNamespace("http://www.example.com"), "lang", "en"))
    }
  }

  describe("validation") {

    it("requires a namespace") {
      intercept[IllegalArgumentException] { new LangSelector(null, "lang", "en") }.getMessage should be("requirement failed: namespace is required")
    }

    it("requires an attribute name") {
      intercept[IllegalArgumentException] { new LangSelector(NoNamespace, null, "en") }.getMessage should be("requirement failed: attrName is required")
    }

    it("requires a language") {
      intercept[IllegalArgumentException] { new LangSelector(NoNamespace, "lang", null) }.getMessage should be("requirement failed: lang is required")
    }

    it("requires a non-empty language") {
      intercept[IllegalArgumentException] { new LangSelector(NoNamespace, "lang", "") }.getMessage should be("requirement failed: lang must not be empty")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = LangSelector("en")
      subject.simplify should be theSameInstanceAs subject
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects all elements with the specified language attribute and language without a namespace") {
        LangSelector(NoNamespace, "lang", "en").select(testXml) should be(trim(<elem1 lang="en" /><elem2 lang="en-us" /><elem1 lang="en"><elem2 /></elem1><elem2 />))
      }

      it("selects all elements with the specified language attribute and language from the specified namespace") {
        LangSelector("http://www.example.com", "lang", "en").select(testXml) should be(trim(<elem1 ns:lang="en" /><elem2 ns:lang="en-us" /><elem1 ns:lang="en"><elem2 /></elem1><elem2 />))
      }

      it("selects all elements with the specified language attribute and language from all namespaces") {
        LangSelector(AnyNamespace, "lang", "en").select(testXml) should be(
          trim(<elem1 lang="en" /><elem2 lang="en-us" /><elem1 lang="en"><elem2 /></elem1><elem2 /><elem1 ns:lang="en" /><elem2 ns:lang="en-us" /><elem1 ns:lang="en"><elem2 /></elem1><elem2 />)
        )
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects all elements with the specified language attribute and language without a namespace") {
        LangSelector(NoNamespace, "lang", "en").select(testXml) should be(
          trim(<elem1 lang="en" /><elem2 lang="en-us" /><elem1 lang="en"><elem2 /></elem1><elem2 /><elem1 LANG="EN" /><elem2 LANG="EN-US" /><elem1 LANG="EN"><elem2 /></elem1><elem2 />)
        )
      }

      it("selects all elements with the specified language attribute and language from the specified namespace") {
        LangSelector("http://www.example.com", "lang", "en").select(testXml) should be(
          trim(<elem1 ns:lang="en" /><elem2 ns:lang="en-us" /><elem1 ns:lang="en"><elem2 /></elem1><elem2 /><elem1 ns:LANG="EN" /><elem2 ns:LANG="EN-US" /><elem1 ns:LANG="EN"><elem2 /></elem1><elem2 />)
        )
      }

      it("selects all elements with the specified language attribute and language from all namespaces") {
        LangSelector(AnyNamespace, "lang", "en").select(testXml) should be(
          trim(
            <elem1 lang="en" /><elem2 lang="en-us" /><elem1 lang="en"><elem2 /></elem1><elem2 /><elem1 LANG="EN" /><elem2 LANG="EN-US" /><elem1 LANG="EN"><elem2 /></elem1><elem2 />
            <elem1 ns:lang="en" /><elem2 ns:lang="en-us" /><elem1 ns:lang="en"><elem2 /></elem1><elem2 /><elem1 ns:LANG="EN" /><elem2 ns:LANG="EN-US" /><elem1 ns:LANG="EN"><elem2 /></elem1><elem2 />
          )
        )
      }
    }
  }
}
