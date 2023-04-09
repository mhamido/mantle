package mhamido.mantle.syntax.parser

import mhamido.mantle.syntax.Decl
import mhamido.mantle.Token
import mhamido.mantle.syntax.Name
import mhamido.mantle.syntax.Pattern

trait DeclParser extends ParserCommons {
  self: ExprParser & PatternParser & TypeParser =>

  private final val Start =
    Seq(Token.Val, Token.DataType, Token.Mutual, Token.Type, Token.Fun)

  def decl(): Decl = expect {
    case Token.Val =>
      val patt = pattern(Token.Eql)
      consume(Token.Eql)
      val body = expr()
      Decl.Val(patt, body)

    case Token.Fun =>
      val name = summon[Token].toName
      val typeParams = peek {
        case Token.OpenBracket =>
          advance()
          val params = typeParamList(List(Token.CloseBracket))
          consume(Token.CloseBracket)
          params

        case _ => Nil
      }

      val args = argumentList()
      val retType = peek {
        case Token.Colon =>
          advance()
          Some(tpe(Seq(Token.Eql)))
        case _ => None
      }

      consume(Token.Eql)
      val body = expr()

      Decl.Function(
        name.asInstanceOf[Name.Var],
        typeParams,
        args,
        retType,
        body
      )

    case Token.DataType =>
      val name = consume(Token.UpperName)
      val tpeParams = typeParamList(List(Token.Eql))
      consume(Token.Eql)
      ???

    case Token.Type =>
      consume(Token.UpperName).map(_.toName).fold(Decl.Mutual(Nil)) { name =>
        val tpeParams = typeParamList(List(Token.Eql))
        consume(Token.Eql)
        val body = tpe(Start)
        Decl.TypeAlias(
          name.asInstanceOf[Name.Constructor],
          tpeParams.asInstanceOf[List[Name.TypeVar]],
          body
        )
      }

    case Token.Mutual =>
      consume(Token.OpenBrace)
      val defs = decls(Token.CloseBrace)
      consume(Token.CloseBrace)
      Decl.Mutual(defs)
  }

  def decls(terminator: Token.Kind): List[Decl] = peek {
    case t if t == terminator => Nil
    case _                    => decl() :: decls(terminator)
  }

  private def argumentList(): List[Pattern] =
    val args = List.newBuilder[Pattern]

    while !isAtEnd && !matches(Token.Colon, Token.Eql) do
      args += simplePattern()

    args.result()

  private def typeParamList(terminators: Seq[Token.Kind]): List[Name] =
    val params = List.newBuilder[Name]
    while !isAtEnd && !matches(terminators: _*) do params += advance().toName
    params.result()
}
