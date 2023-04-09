package mhamido.mantle.syntax.parser

import mhamido.mantle.syntax.Pattern
import mhamido.mantle.Token
import mhamido.mantle.syntax.Name

trait PatternParser extends ParserCommons {
  self: TypeParser =>

  def pattern(terminators: Token.Kind*): Pattern =
    val patt = ascriptionPattern(terminators: _*)
    peek {
      case Token.As =>
        advance()
        consume(Token.LowerName)
          .map(_.toName)
          .fold(patt)(Pattern.Alias(patt, _))
      case _ => patt
    }

  def ascriptionPattern(terminators: Token.Kind*): Pattern =
    val patt = applicationPattern(terminators: _*)
    peek {
      case Token.Colon =>
        advance()
        val typ = tpe(terminators)
        Pattern.Ascribe(patt, typ)
      case _ => patt
    }

  def applicationPattern(terminators: Token.Kind*): Pattern = peek {
    case Token.UpperName =>
      val constructor = advance().toName
      val arguments = List.newBuilder[Pattern]

      while !isAtEnd && !matches(terminators: _*) do
        arguments += simplePattern()

      Pattern.Constructor(constructor, arguments.result())

    case _ =>
      simplePattern()
  }

  def simplePattern(): Pattern = expect {
    case Token.Int    => Pattern.Int(summon[Token].literal.toInt)
    case Token.String => Pattern.String(summon[Token].literal)
    case Token.LowerName =>
      summon[Token].toName match
        case Name.Var("_") =>
          Pattern.Wildcard // TODO: make _ a token on it's own.
        case Name.Var("true")  => Pattern.True
        case Name.Var("false") => Pattern.False
        case name              => Pattern.Var(name)

    case Token.UpperName => Pattern.Constructor(summon[Token].toName, Nil)

    case Token.OpenParen =>
      val patt: Pattern = peek {
        case Token.CloseParen => Pattern.Unit
        case _ =>
          val subpatterns = List.newBuilder[Pattern]
          subpatterns += pattern(Token.Comma, Token.CloseParen)

          while !isAtEnd && matches(Token.Comma) do
            advance()
            subpatterns += pattern(Token.Comma, Token.CloseParen)

          // A pattern like `(p)` should just be equivalent to `p`.
          subpatterns.result() match
            case List(patt) => patt
            case patterns   => Pattern.Tuple(patterns)
      }

      consume(Token.CloseParen)
      patt
  }
}
