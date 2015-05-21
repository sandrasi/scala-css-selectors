package sandrasi.cssselectors.selectors

import org.scalatest.{FunSpec, Matchers}

// TODO (sandrasi): write selection test cases
class NotSelectorTest extends FunSpec with Matchers with Fixture {

  protected lazy override val fixtureFile = "test.xml"

  describe("validation") {

    it("requires a negatable argument") {
      intercept[IllegalArgumentException] { NotSelector(null) }.getMessage should be("requirement failed: argument is required")
    }
  }

  describe("simplification") {

    it("is simplified to an InvalidSelector if its simplified argument is invalid") {
      NotSelector(InvalidSelector).simplify should be(InvalidSelector)
    }
  }
}
