package sandrasi.cssselectors.lexicalscanners

import java.lang.Character._

import org.scalacheck.Gen

import scala.collection.JavaConverters._

object Generators {

  private final val MaxAscii = '\u007f'

  lazy val whitespace = Gen.oneOf(' ', '\t', '\r', '\n', '\f')
  lazy val whitespaces = Gen.nonEmptyListOf[Char](whitespace).map(_.mkString)
  lazy val digits = Gen.listOf(Gen.numChar).map(_.mkString)
  lazy val number = for {
    integerPart <- digits
    fractionalPart <- digits.suchThat(integerPart.nonEmpty || _.nonEmpty).map(d => if (d.nonEmpty) s".$d" else d)
  } yield integerPart + fractionalPart
  lazy val unicodeCodePoint = for {
    highSurrogate <- Gen.choose(MIN_HIGH_SURROGATE, MAX_HIGH_SURROGATE)
    lowSurrogate <- Gen.choose(MIN_LOW_SURROGATE, MAX_LOW_SURROGATE)
    bmpCharBeforeSurrogates <- Gen.choose(Character.MIN_VALUE, (MIN_HIGH_SURROGATE - 1).toChar)
    bmpCharAfterSurrogates <- Gen.choose((MAX_LOW_SURROGATE + 1).toChar, Character.MAX_VALUE)
    bmpChar <- Gen.oneOf(bmpCharBeforeSurrogates, bmpCharAfterSurrogates)
    codePoint <- Gen.frequency(9 -> bmpChar.toInt, 1 -> Character.toCodePoint(highSurrogate, lowSurrogate))
  } yield codePoint
  lazy val asciiCodePoint = Gen.choose(Character.MIN_VALUE, MaxAscii).map(_.toInt)
  lazy val nonAsciiCodePoint = unicodeCodePoint.suchThat(_ > MaxAscii)
  lazy val nonAlphanumericAsciiCodePoint = Gen.oneOf(
    (
      (Character.MIN_VALUE to MaxAscii).toSet &~
      ('a' to 'z').toSet &~
      ('A' to 'Z').toSet &~
      ('0' to '9').toSet &~
      Set('_', '-', '\\')
    ).toSeq
  ).map(_.toInt)
  lazy val unicodeString = Gen.listOf(unicodeCodePoint).map(_.map(asStr).mkString)
  lazy val cssChar = Gen.oneOf(
    Gen.alphaNumChar.map(asStr(_)),
    Gen.const("_"),
    Gen.const("-"),
    nonAsciiCodePoint.map(asStr),
    unicodeCodePointString(),
    escapedChar().suchThat(ec => !nonEscapable.contains(ec.toLowerCase.codePointAt(0)))
  )

  def randomCaseCodePoint(intGen: Gen[Int] = unicodeCodePoint) = intGen.flatMap { codePoint =>
    Gen.oneOf(Character.toLowerCase(codePoint), Character.toUpperCase(codePoint))
  }

  def escapedChar(intGen: Gen[Int] = unicodeCodePoint) = intGen.map { codePoint =>
    val char = asStr(codePoint)
    nonEscapable.find(_ == Character.toLowerCase(codePoint)).map(ne => char).getOrElse("\\" + char)
  }

  def unicodeCodePointString(intGen: Gen[Int] = unicodeCodePoint) = for {
    codePointString <- intGen.map(codePoint => f"$codePoint%x")
    randomCaseCodePointString <- Gen.sequence(codePointString.map(char => randomCaseCodePoint(char.toInt).map(asStr))).map(_.asScala.mkString)
    paddingZeros <- Gen.const("0" * (6 - randomCaseCodePointString.length))
    trailingWhitespace <- Gen.oneOf("", "\r\n", " ", "\t", "\r", "\n", "\f")
  } yield "\\" + paddingZeros + randomCaseCodePointString + trailingWhitespace

  def cssEncodedString(stringGen: Gen[String] = unicodeString) = stringGen.flatMap { string =>
    Gen.sequence(
      codePointsOf(string).map { codePoint =>
        Gen.oneOf(randomCaseCodePoint(codePoint).map(asStr), escapedChar(codePoint), unicodeCodePointString(codePoint))
      }.toList
    ).map(_.asScala.mkString)
  }

  private lazy val nonEscapable = "abcdef0123456789\n\r\f".map(_.toInt).toSet

  private def asStr(codePoint: Int) = new String(Array(codePoint), 0, 1)

  private def codePointsOf(str: String) = str.codePoints.iterator.asScala.map(_.toInt)
}
