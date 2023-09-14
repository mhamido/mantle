package mhamido.mantle.parsers.parser
import org.parboiled2.{Parser, ParseError, Rule1}
import org.parboiled2.ParserInput
import org.parboiled2.{Rule1, Rule0}
import org.parboiled2.ErrorFormatter
import munit.FailException

abstract class TestParserSuite[T] extends munit.FunSuite:
  type ParserType <: TestParser
  def valid: Seq[String]   = Seq.empty
  def invalid: Seq[String] = Seq.empty
  def mkParser(input: ParserInput): ParserType

  abstract class TestParser(val input: ParserInput) extends Parser:
    def Test: Rule1[T]
    def Entry: Rule1[T] = rule(Test ~ EOI)

  private def run(testStr: String): Unit =
    val parser = mkParser(testStr)
    val result = parser.Entry
      .run()
      .recover { case err: ParseError =>
        val msg = parser.formatError(err, new ErrorFormatter(showTraces = true))
        fail(msg, clues(testStr))
      }
      .get
    println(s"\"${testStr}\" => ${result}")

  def run(): Unit =
    valid.foreach: str =>
      test(str):
        run(str)

    invalid.foreach: str =>
      test(str):
        intercept[munit.FailException]:
          run(str)
