package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.configuration.DefaultConfiguration

class ClassSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "/fixtures/class_selector.xml"

  describe("validation") {

    it("requires a class name") {
      intercept[IllegalArgumentException] { ClassSelector(null) }.getMessage should be("requirement failed: className is required")
    }
  }

  describe("simplification") {

    it("can't be further simplified") {
      val subject = ClassSelector("class")
      subject.simplify shouldBe theSameInstanceAs(subject)
    }
  }

  describe("element selection") {

    describe("in a case-sensitive context") {

      it("selects the element with the specified class where the class attribute is not in any namespace") {
        ClassSelector("class1").select(testXml) should be(trim(<elem class="class1 class2" />))
      }
    }

    describe("in a case-insensitive context") {

      implicit val configuration = DefaultConfiguration.withCaseInsensitiveAttributeNames.withCaseInsensitiveAttributeValues

      it("selects the element with the specified class where the class attribute is not in any namespace") {
        ClassSelector("class3").select(testXml) should be(trim(<elem CLASS="CLASS3 CLASS4" />))
      }
    }
  }
}
