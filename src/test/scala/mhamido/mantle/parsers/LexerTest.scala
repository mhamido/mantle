package mhamido.mantle.parsers

import mhamido.SnapshotSuite
import mhamido.mantle.parsing.Lexer
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import mhamido.mantle.parsing.Token

class LexerTest extends SnapshotSuite("lexer") {
  val dir = os.list(os.pwd / "examples")
  for file <- dir do
    snapshot(file) {
      val lexer = Lexer(file)
      val tokens = lexer.toSeq
      tokens
    }
}
