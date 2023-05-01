package mhamido.mantle

import mhamido.mantle.parsing.ModuleParser
import mhamido.mantle.util.{Context, Reporter}
import scala.io.StdIn
import mhamido.mantle.parsing.Parser
import mhamido.mantle.parsing.Token
import scala.util.Try
import scala.collection.StringView

object Main {
  def main(args: Array[String]): Unit = {
    given ctx: Context =
      Context(args.toSeq.map(os.Path(_, os.pwd)), Seq.empty)(using new Reporter)
    if args.nonEmpty then ctx.inputFiles.foreach(compile)
    else repl()
  }

  def repl()(using ctx: Context): Unit = {
    Console.flush()
    val line = StdIn.readLine()
    // if line.headOption.fold(false)(_ == ':') then
    //   val tokens = line.split(' ')
    //   // todo
    // else
    if line.nonEmpty then
      val parser =
        for {
          parser <- ModuleParser(line)
        } yield {
          Try {
            val expr = parser.expr()
            parser.consume(Token.Eof)
            pprint.pprintln(expr, showFieldNames = false)
          }.recover { case ex: Exception =>
            ex.printStackTrace()
            ctx.reporter.info(
              parser.tokens.map(_.kind).toList.mkString("{", ",", "}")
            )
          }
        }
      repl()
  }

  def compile(srcpath: os.Path)(using rep: Context): Unit = {
    val module = ModuleParser(srcpath)
    rep.reporter.exitIfErrors()
    pprint.pprintln(module, showFieldNames = false)
  }
}
