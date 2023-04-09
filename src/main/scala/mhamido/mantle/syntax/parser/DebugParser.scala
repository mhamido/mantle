package mhamido.mantle.syntax.parser

import mhamido.mantle.Token
import mhamido.mantle.util.Reporter
import mhamido.mantle.util.Position
import mhamido.mantle.util.Context

object DebugParser {
  private final val DebugContext = Context(
    inputFiles = Nil,
    outFiles = Nil,
    dumpSyntaxTree = true,
    dumpTokens = false,
    optimize = false
  )(using new Reporter())
}
