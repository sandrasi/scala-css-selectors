package sandrasi.cssselectors.lexicalscanners

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}
import sandrasi.cssselectors.lexicalscanners.Generators._
import sandrasi.cssselectors.lexicalscanners.SelectorTokenizer._

import scala.collection.JavaConverters._
import scala.util.matching.Regex

class SelectorTokenizerTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks with SharedTokenizerBehaviors {

  describe("S") {
    it should behave like matchingTokenizerRegex(S, whitespaces)
    it should behave like nonMatchingTokenizerRegex(S, Arbitrary.arbString.arbitrary, str => str.nonEmpty && str.trim.isEmpty)
  }

  describe("IDENT") {

    lazy val identGen = Gen.nonEmptyListOf(cssChar).map(_.mkString).suchThat(_.matches("""(?s)[^0-9-].*"""))

    describe("starts with alphabetic, or underscore, or non-ascii, or escaped, or unicode code point character") {

      it should behave like matchingTokenizerRegex(IDENT, identGen)
    }

    describe("starts with a hyphen, followed by a character that is neither a hyphen nor a digit") {

      it should behave like matchingTokenizerRegex(IDENT, identGen.map('-' + _))
    }

    describe("contains escaped line feed, carriage return, or form feed characters") {
      val foo = for {
        ident <- identGen
        pos <- Gen.choose(0, ident.length).suchThat(ident(_) != '\\')
        lfCrFf <- Gen.oneOf('\n', '\r', '\f')
      } yield {println("'" + ident + "', length: " + ident.length + ", pos: " + pos);ident.substring(0, pos) + lfCrFf + ident.substring(pos, ident.length)}

      it should behave like nonMatchingTokenizerRegex(IDENT, foo)
    }

//    describe("contains ascii characters that are neither of letters, digits, underscore, and hyphen") {
//
//      val foo = for {
//        identStr <- identStrGen.map(_.codePoints.asS)
//        position <- Gen.choose(0, identStr.length)
//      }
//
//      it should behave like nonMatchingTokenizerRegex(IDENT, foo)
//        forAll(
//          Gen.frequency(9 -> Gen.alphaChar, 1 -> '_'),
//          Gen.nonEmptyListOf(codePointAsString(nonAlphanumericAsciiCodePoint)).map(_.mkString)
//        ) { (start, tail) =>
//          (start + tail) should not fullyMatch regex(IDENT)
//        }
//    }
  }

  describe("STRING") {

  }

  describe("FUNCTION") {

  }

  describe("NUMBER") {

    it should behave like matchingTokenizerRegex(NUMBER, number)

    describe("incomplete") {
      it should behave like nonMatchingTokenizerRegex(NUMBER, digits.suchThat(_.nonEmpty).map(_ + '.'))
    }

    describe("non-number") {
      it should behave like nonMatchingTokenizerRegex(NUMBER, Arbitrary.arbString.arbitrary, _.matches("""\d+|\d*\.\d+"""))
    }
  }

  describe("HASH") {

  }

  describe("PLUS") {
    it should behave like matchingTokenizerRegex(PLUS, whitespaces.flatMap(_ + '+'))
    it should behave like nonMatchingTokenizerRegex(PLUS, cssEncodedString(), _.trim == "+")
  }

  describe("GREATER") {
    it should behave like matchingTokenizerRegex(GREATER, whitespaces.flatMap(_ + '>'))
    it should behave like nonMatchingTokenizerRegex(GREATER, cssEncodedString(), _.trim == ">")
  }

  describe("COMMA") {
    it should behave like matchingTokenizerRegex(COMMA, whitespaces.flatMap(_ + ','))
    it should behave like nonMatchingTokenizerRegex(COMMA, cssEncodedString(), _.trim == ",")
  }

  describe("TILDE") {
    it should behave like matchingTokenizerRegex(TILDE, whitespaces.flatMap(_ + '~'))
    it should behave like nonMatchingTokenizerRegex(TILDE, cssEncodedString(), _.trim == "~")
  }

  describe("NOT") {
    it should behave like matchingTokenizerRegex(NOT, cssEncodedString("not"))
    it should behave like nonMatchingTokenizerRegex(NOT, cssEncodedString(), decode(_).equalsIgnoreCase("not"))
  }

  describe("DIMENSION") {

  }

  describe("INTEGER") {
    it should behave like matchingTokenizerRegex(INTEGER, digits.suchThat(_.nonEmpty))
    it should behave like nonMatchingTokenizerRegex(INTEGER, Arbitrary.arbString.arbitrary, _.matches("""\d+"""))
  }

  describe("N") {
    it should behave like matchingTokenizerRegex(N, cssEncodedString("n"))
    it should behave like nonMatchingTokenizerRegex(N, cssEncodedString(), decode(_).equalsIgnoreCase("n"))
  }

  describe("ODD") {
    it should behave like matchingTokenizerRegex(ODD, cssEncodedString("odd"))
    it should behave like nonMatchingTokenizerRegex(ODD, cssEncodedString(), decode(_).equalsIgnoreCase("odd"))
  }

  describe("EVEN") {
    it should behave like matchingTokenizerRegex(EVEN, cssEncodedString("even"))
    it should behave like nonMatchingTokenizerRegex(EVEN, cssEncodedString(), decode(_).equalsIgnoreCase("even"))
  }

  describe("decode") {

    it("does not change normal characters") {
      forAll(unicodeString) { str => whenever(!str.contains('\\')) { decode(str) should be(str) } }
    }

    it("unescapes the escaped characters") {
      forAll(unicodeString) { str => decode(escapedCharsOf(str)) should be(str) }
    }

    it("decodes the unicode values to real characters") {
      forAll(unicodeString) { str => decode(codePointStringFrom(str)) should be(str) }
    }
  }

  private def escapedCharsOf(str: String) = stringMapper(str) { escapedChar(_).sample.get }

  private def codePointStringFrom(str: String) = stringMapper(str) { unicodeCodePointString(_).sample.get }

  private def stringMapper(str: String)(codePointMapper: Int => String) = codePointsOf(str).map(codePointMapper).toList.mkString

  private def codePointsOf(str: String) = str.codePoints.iterator.asScala.map(_.toInt)

  private def codePointAsString = (intGen: Gen[Int]) => intGen.map(codePoint => new String(Array(codePoint), 0, 1))
}

trait SharedTokenizerBehaviors { this: FunSpec with Matchers with GeneratorDrivenPropertyChecks =>

  def matchingTokenizerRegex(token: Regex, stringGen: Gen[String]) {

    it("matches the string against the regex") {
      forAll(stringGen) { _ should fullyMatch regex token }
    }
  }

  def nonMatchingTokenizerRegex(token: Regex, stringGen: Gen[String], exception: String => Boolean = str => false) {

    it("mismatches every string but the exception") {
      forAll(stringGen) { str => whenever(!exception(str)) { str should not fullyMatch regex(token) } }
    }
  }
}
