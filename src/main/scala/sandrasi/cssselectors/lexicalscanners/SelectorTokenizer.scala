package sandrasi.cssselectors.lexicalscanners

import java.util.regex.Matcher

import scala.util.matching.Regex

/** Provides a set of strings and regular expressions to tokenize the CSS3 selectors. The tokenizer is
  * ''case-insensitive''.
  *
  * == Terminology ==
  *
  * '''Whitespace character''' The ''space'' (U+0020), the ''horizontal tab'' (U+0009), the ''form feed'' (U+000C), the
  * ''carriage return'' (U+000D), and the ''line feed'' (U+000A) characters are whitespace characters.
  *
  * '''Unicode code point string''' The character's hexadecimal Unicode code point value as a string, prepended by a
  * ''backslash'' (U+005C). The hexadecimal number may be left-padded with zeros up to six digits. It may be followed by
  * a single whitespace character or by the combination of a ''carriage return'' (U+000D) and a ''line feed'' (U+000A).
  *
  * '''Escaped character''' Any character except the hexadecimal digits (from ''0'' to ''9'' and from ''a'' to ''f''),
  * the ''line feed'' (U+000A), the ''carriage return'' (U+000D), and the ''form feed'' (U+000C) can be escaped with a
  * ''backslash'' (U+005C).
  *
  * '''Decimal number''' A non-negative number that have a fractional part separated from the integer part with a
  * decimal separator. If the number is less than 1, the leading 0 can be omitted.
  */
object SelectorTokenizer {

  /** Matches on a series of whitespace characters. */
  final lazy val S: Regex = """[ \t\r\n\f]+""".r

  /** Represents the ''including'' attribute selector operator (~=). */
  final lazy val INCLUDES: String = "~="

  /** Represents the ''dash-matching'' attribute selector operator (|=). */
  final lazy val DASH_MATCH: String = "|="

  /** Represents the ''prefix-matching'' attribute selector operator (^=). */
  final lazy val PREFIX_MATCH: String = "^="

  /** Represents the ''suffix-matching'' attribute selector operator ($=). */
  final lazy val SUFFIX_MATCH: String = "$="

  /** Represents the ''containing'' attribute selector operator (*=). */
  final lazy val SUBSTRING_MATCH: String = "*="

  /** Matches on an arbitrary, non-empty string identifier. It must start with either an alphabetic character, or an
    * ''underscore'' (U+005F), or a character with Unicode code point value greater than U+007F, or a Unicode code point
    * string, or an escaped character. It may be prepended by a ''hyphen'' (U+002D) and is optionally followed by
    * similar set of characters, Unicode code point strings, escaped characters, hyphens, and digits.
    */
  final lazy val IDENT: Regex = raw"(?i)$ident".r

  /** Matches on an arbitrary string between a pair of ''apostrophe''s (U+0027) or ''quotation mark''s (U+0022). The
    * string may be empty or may contain arbitrary characters, Unicode code point strings, or escaped characters. ''Line
    * feed''s (U+000A), ''carriage return''s (U+000D), ''form feed''s (U+000C), combination of a ''carriage
    * return'' (U+000D) and a ''line feed'' (U+000A), quoting characters, and ''backslash''es (U+005C) are only allowed
    * in the string if they are escaped with an extra ''backslash'' (U+005C).
    */
  final lazy val STRING: Regex = raw"(?i)$string".r

  /** Matches on a function identifier. The identifier is an
    * [[sandrasi.cssselectors.lexicalscanners.SelectorTokenizer.IDENT IDENT]] immediately followed by a ''left
    * parenthesis'' (U+0028).
    */
  final lazy val FUNCTION: Regex = raw"""(?i)$ident\(""".r

  /** Matches on a decimal number. */
  final lazy val NUMBER: Regex = num.r

  /** Matches on an arbitrary, non-empty string name prepended by a ''number sign'' (U+0023). The string may contain
    * alphanumeric characters, ''hyphen''s (U+002D), ''underscore''s (U+005F), characters with Unicode code point value
    * greater than U+007F, Unicode code point strings, or escaped characters.
    */
  final lazy val HASH: Regex = raw"(?i)#$name".r

  /** Matches on the ''plus sign'' adjacent sibling combinator (+) that may be prepended by a series of whitespace
    * characters.
    */
  final lazy val PLUS: Regex = raw"""(?:$w)\+""".r

  /** Matches on the ''greater-than sign'' child combinator (>) that may be prepended by a series of whitespace
    * characters.
    */
  final lazy val GREATER: Regex = raw"(?:$w)>".r

  /** Matches on the ''comma'' (U+002C) selector separator (,) that may be prepended by a series of whitespace
    * characters.
    */
  final lazy val COMMA: Regex = raw"(?:$w),".r

  /** Matches on the ''tilde operator'' general sibling combinator (~) that may be prepended by a series of whitespace
    * characters.
    */
  final lazy val TILDE: Regex = raw"(?:$w)~".r

  /** Matches on the keyword ''not''. Each character of the keyword may be represented by the character itself or by the
    * character's Unicode code point string. Each of the characters may also be escaped.
    */
  final lazy val NOT: Regex = raw"(?i)(?:$n)(?:$o)(?:$t)".r

  /** Matches on a decimal number immediately followed by an
    * [[sandrasi.cssselectors.lexicalscanners.SelectorTokenizer.IDENT IDENT]] identifier.
    */
  final lazy val DIMENSION: Regex = raw"(?i)(?:$num)(?:$ident)".r

  /** Matches on an integer number. */
  final lazy val INTEGER: Regex = "[0-9]+".r

  /** Matches on the keyword ''n''. The keyword may be represented by the ''n'' character itself or by its Unicode code
    * point string. It may also be escaped.
    */
  final lazy val N: Regex = raw"(?i)(?:$n)".r

  /** Matches on the keyword ''odd''. Each character of the keyword may be represented by the character itself or by the
    * character's Unicode code point string. The ''o'' character may also be escaped.
    */
  final lazy val ODD: Regex = raw"(?i)(?:$o)(?:$d)(?:$d)".r

  /** Matches on the keyword ''even''. Each character of the keyword may be represented by the character itself or by
    * the character's Unicode code point string. The ''n'' and ''v'' characters may also be escaped.
    */
  final lazy val EVEN: Regex = raw"(?i)(?:$e)(?:$v)(?:$e)(?:$n)".r

  /** Converts a string including ''escaped character''s and ''Unicode code point string''s to a string in which the
    * escaped characters and Unicode code point values are resolved to actual characters.
    */
  final def decode(escapeString: String): String = raw"(?i)$escape".r("unicode", "escape").replaceSomeIn(
    escapeString,
    m => Option(m.group("unicode")).map { unicode =>
      Matcher.quoteReplacement(new String(Array(Integer.parseInt(unicode.drop(1).trim, AsHexadecimal)), 0, 1))
    }.orElse(
      Option(m.group("escape")).map { escape =>
        Matcher.quoteReplacement(escape.drop(1))
      }
    )
  )

  private final val AsHexadecimal = 16

  private final val nonAscii = """[^\x00-\x7f]"""
  private final val unicode = """\\[0-9a-f]{1,6}(?:\r\n|[ \n\r\t\f])?"""
  private final val escape = raw"""($unicode)|(\\[^\n\r\f0-9a-f])"""
  private final val nmStart = raw"[_a-z]|(?:$nonAscii)|(?:$escape)"
  private final val nmChar = raw"[_a-z0-9-]|(?:$nonAscii)|(?:$escape)"
  private final val ident = raw"[-]?(?:$nmStart)(?:$nmChar)*"
  private final val name = raw"(?:$nmChar)+"
  private final val num = """[0-9]+|[0-9]*\.[0-9]+"""
  private final val nl = """\n|\r\n|\r|\f"""
  private final val string1 = raw""""(?:[^\n\r\f\\"]|\\(?:$nl)|(?:$nonAscii)|(?:$escape))*""""
  private final val string2 = raw"""'(?:[^\n\r\f\\']|\\(?:$nl)|(?:$nonAscii)|(?:$escape))*'"""
  private final val string = raw"($string1)|($string2)"
  private final val w = """[ \t\r\n\f]*"""
  private final val d = """[d]|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?"""
  private final val e = """[e]|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?"""
  private final val n = """[n]|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\[n]"""
  private final val o = """[o]|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\[o]"""
  private final val t = """[t]|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\[t]"""
  private final val v = """[v]|\\0{0,4}(56|76)(\r\n|[ \t\r\n\f])?|\\[v]"""
}
