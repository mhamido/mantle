package mhamido.mantle.syntax.parser

import mhamido.mantle.Lexer
import mhamido.mantle.Token
import mhamido.mantle.syntax.Decl
import mhamido.mantle.syntax.Name
import mhamido.mantle.util.Context
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Position
import mhamido.mantle.util.Reporter
import mhamido.mantle.syntax.parser.*
import os.Path

class ModuleParser(val path: Path, val tokens: BufferedIterator[Token])(using
    override val reporter: Reporter
) extends ParserCommons,
      TypeParser,
      ExprParser,
      DeclParser,
      PatternParser {
  def module() =
    val name = peek {
      case Token.Module =>
        advance()
        val parts = List.newBuilder[Name]
        val name = consume(Token.UpperName).getOrElse(Name.Error)

        while !isAtEnd && matches(Token.Dot) do
          advance()
          consume(Token.UpperName).foreach(parts += _.toName)

        parts.result()
      case _ => Seq(ModuleParser.MainName)
    }

    mhamido.mantle.syntax.Module(name, decls(Token.Eof))
}

object ModuleParser extends Phase[BufferedIterator[Token], Nothing] {
  private[ModuleParser] final val MainName = Name.Constructor("Main")

  override def apply(in: BufferedIterator[Token])(using Context): Nothing = ???

  def debug(in: String): (ModuleParser, Reporter) = {
    given Reporter = new Reporter() {
      override inline def fatalError[A](msg: A, pos: Position): Nothing =
        report(Reporter.Error, msg, pos)
        throw msg.asInstanceOf[Throwable]
    }

    val input = in.iterator.buffered
    val path = os.temp()
    val lexer = new Lexer(path, input)
    val mod = new ModuleParser(path, lexer.iterator.buffered)
    (mod, summon[Reporter])
  }
}
