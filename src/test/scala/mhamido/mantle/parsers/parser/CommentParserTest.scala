package mhamido.mantle.parsers.parser

import org.parboiled2.{Rule0, Rule1}
import org.parboiled2.ParserInput
import org.parboiled2.ParseError
import mhamido.mantle.parsing.BaseParser
import org.parboiled2.ErrorFormatter

class CommentParserTest extends TestParserSuite[Int]:
  class ParserType(override val input: ParserInput)
      extends TestParser(input),
        BaseParser:
    def Test = rule(Comment.+ ~ push(0))

  def mkParser(input: ParserInput) = ParserType(input)
  override def valid: Seq[String] = List(
    "/* Basic Comment */",
    "/* Consecutive *//* Comments */",
    "/* Possibly / Confusing * Comment */",
    "/* Nested /* Comment */ */",
    "/* Another /* Nested */ Comment */",
    "/* Deeply /* Nested /* Comment */ */ */"
  )

  override def invalid: Seq[String] = List(
    "/* Unclosed comment",
    "Unopened Comment */",
    "/* Deeply /* nested */ but unclosed",
    "/* Another /* nested comment but unclosed */"
  )

  run()
