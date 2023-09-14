package mhamido.mantle

import mhamido.mantle.parsing.ModuleParser
import mhamido.mantle.util.{Context, Reporter}
import scala.io.StdIn
import scala.util.Try
import scala.collection.StringView
import java.nio.charset.Charset
import org.parboiled2.ParseError
import org.parboiled2.ErrorFormatter

object Main {
  def main(args: Array[String]): Unit = {
    given ctx: Context =
      Context(args.toIndexedSeq.map(os.Path(_, os.pwd)), Seq.empty)(using
        new Reporter
      )
    if args.nonEmpty then ctx.inputFiles.foreach(compile)
    else repl()
  }

  def repl()(using ctx: Context): Unit = {
    val line = StdIn.readLine("mantle>")
    if line.nonEmpty then
      val term = ModuleParser(line)
      term.fold(
        println(_),
        pprint.pprintln(_)
      )
      repl()
  }

  def compile(srcpath: os.Path)(using rep: Context): Unit = {
    val module = ModuleParser(srcpath)
    pprint.pprintln(module, showFieldNames = false)
  }
}
