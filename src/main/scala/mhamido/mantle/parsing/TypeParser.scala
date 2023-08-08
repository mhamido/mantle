package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.util.Position

trait TypeParser extends Parser {
  def tpe(): Type = peek {
    case Token.OpenBracket =>
      val openBracket = consume(Token.OpenBracket).get
      expect { case Token.PrimedName =>
        val name = summon[Token].literal
        consume(Token.CloseBracket)
        consume(Token.ThinArrow)
        val body = tpe()
        Type.TypeFn(name, body)(using openBracket.pos <> body.info)
      }

    case _ =>
      val fromType = primType()
      peek {
        case Token.ThinArrow =>
          val arrow  = advance()
          val toType = tpe()
          Type.Fn(fromType, toType)(using fromType.info <> toType.info)
        case _ => fromType
      }
  }

  private def primType(): Type = peek {
    case Token.OpenParen =>
      val (openParen, types, closeParen) =
        between(Token.OpenParen, Token.Comma, Token.CloseParen)(tpe)
      Type.Tuple(types)(using ???)
    case _ => namedType()
  }

  private def namedType(): Type = expect {
    case Token.PrimedName =>
      val token = summon[Token]
      Type.Var(token.literal)(using token.pos <> pos)

    case Token.UpperName | Token.LowerName =>
      val token = summon[Token]
      TypeParser.primOrNamedType(token.literal)(using token.pos <> pos)
  }

  private def parameterizedType(): Type = {
    def loop(head: Type): Type = peek {
      case Token.OpenBracket =>
        // loop(Type.Apply(head, paramList()))
        ???
      case _ => head
    }
    loop(primType())
  }

  private def paramList() =
    between(Token.OpenBracket, Token.Comma, Token.CloseBracket)(tpe)

}

object TypeParser {
  import syntax.{Type, Info}

  def primOrNamedType(name: String)(using Info) = name match {
    case "Int"    => Type.Int()
    case "Unit"   => Type.Unit()
    case "String" => Type.String()
    case "Array"  => Type.Array()
    case "Bool"   => Type.Bool()
    case _        => Type.Named(name)
  }
}
