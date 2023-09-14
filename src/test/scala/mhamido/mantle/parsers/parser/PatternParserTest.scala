package mhamido.mantle.parsers.parser

import mhamido.mantle.syntax
import org.parboiled2.ParserInput
import mhamido.mantle.parsing.BaseParser
import org.parboiled2.Rule1
import mhamido.mantle.parsing.PatternParser
import mhamido.mantle.parsing.TypeParser

class PatternParserTest extends TestParserSuite[syntax.Pattern]:
  class ParserType(override val input: ParserInput)
      extends TestParser(input),
        BaseParser,
        PatternParser,
        TypeParser:
    override def Test: Rule1[syntax.Pattern] = rule(Pattern)

  override def mkParser(input: ParserInput): ParserType = ParserType(input)

  override def valid: Seq[String] = List(
    "()",
    "0",
    "123",
    "x",
    "'a'",
    "(foo)",
    "\"Hello, World!\"",
    "(x, y)",
    "(1, 'a', 3)",
    "(x, y)",
    // "(x, 'a', 10) as foo",
    // "bar as baz",
    // "(bart: Simpson) as alias",
    "Point (x, y)",
    "(x: Float, y: Float)",
    "Point (x: Float, y: Float)",
    "Pair [Int] xs",
    "Pair ['a, 'b] (x: 'a, y: 'b)"
  )

  override def invalid: Seq[String] =  List(
    "",
    "(", ")",
    "(123", ")123",
    "123x",
    "a'",
    "\"Hello, World!", "Hello, World!\"",
    "(x, y", "y, x)",
    "(1, a', 3)",
    "(x, y)",
    "(x, 'a', 10)",
    "(: Simpson)",
    "Point (x, y", "Point x, y)",
    "Point (x Float, y Float)",
    "Pair [Int (x, y)",
    "Pair Int] xs"
  )

  run()
