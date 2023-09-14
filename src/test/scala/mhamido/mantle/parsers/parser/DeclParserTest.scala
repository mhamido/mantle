package mhamido.mantle.parsers.parser

import org.parboiled2.ParserInput
import mhamido.mantle.parsing.BaseParser
import mhamido.mantle.parsing.PatternParser
import mhamido.mantle.parsing.DeclParser
import org.parboiled2.Rule1
import mhamido.mantle.syntax
import mhamido.mantle.parsing.TypeParser
import mhamido.mantle.parsing.ExprParser

class DeclParserTest extends TestParserSuite[syntax.Decl]:

  override def mkParser(input: ParserInput): ParserType = ParserType(input)

  class ParserType(override val input: ParserInput)
      extends TestParser(input),
        BaseParser,
        ExprParser,
        PatternParser,
        TypeParser,
        DeclParser:
    override def Test: Rule1[syntax.Decl] = rule(Decl)

  override def valid: Seq[String] = List(
    "val x = 10",
    "val x: Int = 20",
    "val (x, y) = (10, 20)",
    "type Foo['a, 'b] = Bar ['b, 'a]",
    "datatype Option['a] = None | Some 'a",
    "datatype Color = Red | Green | Blue",
    "fun identity ['a] (x: 'a): 'a = x",
    "fun const ['a] ['b] (x: 'a) (y: 'b): 'a = x",
    "fun pair ['a] ['b] (x: 'a) (y: 'b): ('a, 'b) = (x, y)",
    "fun foo ['a] (x: 'a): 'a = bar ['a] x and bar ['b] (y: 'b): 'b = foo ['a] y"
  )

  run()
