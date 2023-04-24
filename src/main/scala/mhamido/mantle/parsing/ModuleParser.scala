package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.util.Reporter
import mhamido.mantle.parsing.Lexer

import scala.util.Try
import mhamido.mantle.parsing.Parser

class ModuleParser(
    override val tokens: BufferedIterator[Token]
)(using override val reporter: Reporter)
    extends Parser,
      DeclParser,
      ExprParser,
      TypeParser,
      PatternParser {
  def module(): syntax.Module = {
    consume(Token.Module)
    val name = qualifiedName()
    consume(Token.With)
    val defs = decls(Token.Eof)
    consume(Token.Eof)
    syntax.Module(name, defs)
  }

  def qualifiedName(): Seq[syntax.Name] =
    val names = List.newBuilder[syntax.Name]
    consume(Token.UpperName).foreach(names += _.literal)

    while !isAtEnd && !matches(Token.With) do
      consume(Token.Dot)
      consume(Token.UpperName).foreach(names += _.literal)

    names.result()
}

object ModuleParser {
  def apply(path: os.Path)(using rep: Reporter): Try[syntax.Module] = Try {
    val tokens = Lexer(path)
    val parser = new ModuleParser(tokens)
    parser.module()
  }

  def apply(str: String)(using rep: Reporter): Try[ModuleParser] = Try {
    val tokens = new Lexer(os.temp(), str.iterator.buffered)
    new ModuleParser(tokens.iterator.buffered)
  }
}
