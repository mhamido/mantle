package mhamido.mantle

import mhamido.mantle.util.Context
import mhamido.mantle.util.Reporter
import mhamido.mantle.syntax.parser.ModuleParser

class ModuleParseSuite extends munit.FunSuite {
  os.list(os.pwd / "examples").foreach { srcpath =>
    test(s"$srcpath") {
      given ctx: Context = Context(Seq(srcpath), Seq())(using new Reporter)
      if !os.exists(srcpath) then
        ctx.reporter.error(s"File $srcpath does not exist.")
      else
        val tokens = Lexer(srcpath)
        val module = ModuleParser(srcpath, tokens.iterator.buffered)
        pprint.pprintln(module)
      assert(!ctx.reporter.hadErrors)
    }
  }
}
