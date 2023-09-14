package mhamido.mantle.parsers.parser

import mhamido.mantle.parsing.TypeParser
import mhamido.mantle.syntax
import org.parboiled2.ParserInput
import org.parboiled2.{Rule0}

import scala.util.Failure
import scala.util.Try
import org.parboiled2.ParseError
import mhamido.mantle.parsing.BaseParser

class TypeParserTest extends TestParserSuite[syntax.Type]:
  class ParserType(override val input: ParserInput)
      extends TestParser(input),
        TypeParser,
        BaseParser:
    def Test = rule(Type ~ EOI)
  override def mkParser(input: ParserInput): ParserType = ParserType(input)
  override def valid: Seq[String] = List(
    "['a] -> 'a -> 'a",
    "(Int, Int) -> Bool",
    "Foo -> Bar -> Baz -> Boo",
    "['a] -> ['b] -> 'a",
    "List [Int, Bar]",
    "['a] -> ['b] -> ('a -> 'b) -> List['a] -> List ['b]",
    "['a] -> Int",
    "['a] -> ('b)",
    "['a] -> (['b] -> 'c)",
    "Int -> (String -> Char)",
    "(Int -> String) -> (Char -> Bool)",
    "(Int -> (String -> Char)) -> ((Bool -> Char) -> (Float -> Double))",
    "['a] -> (Int -> 'a)",
    "(Int -> String) -> (['b] -> 'c)",
    "['a] -> (['b] -> (['c] -> 'd))",
    "Int -> ('a -> 'b) -> Int -> Char",
    "(Int -> String) -> ('a -> Bool) -> Char -> Float",
    "'a -> 'b",
    "Int -> String"
  )
  run()