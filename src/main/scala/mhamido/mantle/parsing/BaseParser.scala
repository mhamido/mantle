package mhamido.mantle.parsing

import mhamido.mantle.util.Span
import org.parboiled2.*
import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either

trait BaseParser extends Parser:
  export mhamido.mantle.syntax

  given Conversion[String, Rule0] with
    def apply(x: String): Rule0 = rule:
      atomic(str(x)) ~ WS

  def WS: Rule0 = rule:
    quiet((CharPredicate.from(_.isWhitespace) | Comment).*)

  def Comment: Rule0 =
    def E: Rule0 = rule(!("*/") ~ (Comment | ANY))
    rule(str("/*") ~ zeroOrMore(E) ~ str("*/"))

  def UpperName: Rule1[String] = rule:
    capture(
      CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum | '_')
    ) ~ WS

  def LowerName: Rule1[String] = rule:
    capture(CharPredicate.LowerAlpha ~ (CharPredicate.AlphaNum | '_').*) ~> {
      name => test(!BaseParser.Keywords.contains(name)) ~ push(name)
    } ~ WS

  def PrimedName: Rule1[String] = rule:
    '\'' ~ LowerName ~> ('\'' +: _)

  def Integer: Rule1[Int] =
    def nonZero = rule:
      capture(CharPredicate.Digit19) ~ capture(CharPredicate.Digit.*) ~> (_ + _)
    rule((nonZero | capture(CharPredicate.Digit)) ~ WS ~> (_.toInt))

  def CharLiteral: Rule1[String] = rule:
    '\'' ~ capture(!('\'') ~ ANY) ~ '\'' ~ WS
    // capture('\'' ~ (!('\'') ~ ANY) ~ '\'') ~ WS

  def StringLiteral: Rule1[String] = rule:
    '"' ~ capture((!('"') ~ ANY).*) ~ '"' ~ WS

object BaseParser:
  final val Keywords = Set(
    "module",
    "with",
    "import",
    "fun",
    "val",
    "let",
    "end",
    "type",
    "datatype",
    "in",
    "fn",
    "foreign",
    "if",
    "then",
    "and",
    "else",
    "true",
    "false",
    // Reserved but unimplemented
    "interface",
    "instance"
  )
