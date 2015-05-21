package sandrasi.cssselectors.selectors

import org.scalatest.{Matchers, FunSpec}

class SimpleSelectorSequenceSelectorTest extends FunSpec with Matchers with Fixture {

  override protected def fixtureFile = "/fixtures/simple_selector_sequence_selector.xml"

  describe("validation") {

    it("requires an element selector") {
      intercept[IllegalArgumentException] { new SimpleSelectorSequenceSelector(null)() }.getMessage should be("requirement failed: elementSelector is required")
    }

    it("requires chainable selectors") {
      intercept[IllegalArgumentException] { new SimpleSelectorSequenceSelector(None, null)() }.getMessage should be("requirement failed: chainableSelectors should not contain null elements")
    }

    it("requires a pseudo element selector") {
      intercept[IllegalArgumentException] { new SimpleSelectorSequenceSelector(None)(null) }.getMessage should be("requirement failed: pseudoElementSelector is required")
    }

    it("requires at least one selector") {
      intercept[IllegalArgumentException] { new SimpleSelectorSequenceSelector(None)(None) }.getMessage should be("requirement failed: at least one selector is required")
    }
  }

  describe("simplification") {

    describe("element selector is not specified") {

      it("can be simplified to a single chainable selector") {
        SimpleSelectorSequenceSelector(RootSelector()).simplify should be(RootSelector())
      }

      ignore("can be simplified to a single pseudo element selector") {
        SimpleSelectorSequenceSelector(FirstLineSelector()).simplify should be(FirstLineSelector())
      }

      describe("a selector is invalid") {

        it("is simplified to an InvalidSelector if a chainable selector is invalid") {
          SimpleSelectorSequenceSelector(InvalidSelector.asInstanceOf[ChainableSelector]).simplify should be(InvalidSelector)
        }

        it("is simplified to an InvalidSelector if the pseudo element selector is invalid") {

        }
      }
    }

    it("can be simplified to a single element selector") {
      SimpleSelectorSequenceSelector(UniversalSelector()).simplify should be(UniversalSelector())
    }
  }
}
