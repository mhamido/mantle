package mhamido.mantle

import mhamido.mantle.util.Context

import mainargs.ParserForClass
import mainargs.TokensReader
import scala.io.StdIn
import scala.util.control.NonFatal
import mhamido.mantle.util.Reporter
import mhamido.mantle.parser.ModuleParser

object Main:
  given TokensReader[os.Path]("path", strs => Right(os.Path(strs.head, os.pwd)))

  def main(args: Array[String]): Unit =
    given ctx: Context = ParserForClass[Context].constructOrExit(args.toSeq)
    if ctx.inputFiles.isEmpty then repl(using ctx.reporter)
    else ctx.inputFiles.foreach(compile(_))

  def repl(using rep: Reporter): Unit =
    try {
      val line = StdIn.readLine()
      println(line)
    } catch {
      case NonFatal(ex) => rep.error(ex)
    }

  def compile(srcpath: os.Path)(using ctx: Context): Unit =
    val tokens = Lexer(srcpath)
    val ast = ModuleParser(srcpath, tokens)
    