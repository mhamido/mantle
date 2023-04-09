package mhamido.mantle

import mhamido.mantle.util.Context

import mainargs.ParserForClass
import mainargs.TokensReader

object Main:
  given TokensReader[os.Path]("path", strs => Right(os.Path(strs.head, os.pwd)))

  def main(args: Array[String]): Unit =
    given ctx: Context = ParserForClass[Context].constructOrExit(args.toSeq)
    val tokens = ctx.inputFiles.map { srcpath =>
      (srcpath, Lexer(srcpath))
    }
