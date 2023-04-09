package mhamido.mantle

import mhamido.mantle.util.Context
import mhamido.mantle.syntax.parser.ModuleParser

class Driver(using ctx: Context) {
  def compile(): Unit = {
  }

  val pipeline = Lexer andThen ModuleParser 
}
