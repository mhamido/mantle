package mhamido.mantle.parsers.parser

import mhamido.mantle.syntax
import org.parboiled2.ParserInput
import mhamido.mantle.parsing.BaseParser
import org.parboiled2.Rule1
import mhamido.mantle.parsing.PatternParser
import mhamido.mantle.parsing.TypeParser
import mhamido.mantle.parsing.ExprParser
import mhamido.mantle.parsing.DeclParser

class ExprParserTest extends TestParserSuite[syntax.Expr]:
    override def mkParser(input: ParserInput): ParserType = ParserType(input)

    class ParserType(override val input: ParserInput) extends TestParser(input), ExprParser, PatternParser, BaseParser, TypeParser, DeclParser:
        def Test: Rule1[syntax.Expr] = rule(Expr)

    override def valid: Seq[String] = List(
        "()",
        "10", 
        "x",
        "true",
        "(10, x, true)",
        "let val x = 10 val y = 20 in x + y",
        "fn (x: Int) => match x with 0 => true | 1 => false end",
        "fn ['a] (x: 'a) => x",
        """fn ['a] ['b] (f: 'a -> 'b) (xs: Option['a]) => 
            match xs with 
            | None              => None 
            | Some ['a] (x: 'a) => some (f x)
            end""",
        "1 + 2",
        "1 + 2 + 3",
        "1 + 2 * 4",
        "f x y z + g y * h z",
        "(if foo then const else discard) x y * 1"
        
    )

    run()