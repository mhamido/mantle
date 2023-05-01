package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.util.Position

trait TypeParser extends Parser {
  def tpe(tokens: Seq[Token.Kind] = Seq.empty): Type = {
    val base = typeApplication()
    peek {
      case Token.ThinArrow =>
        val arrow = advance()
        Type.Fn(base, tpe())(using ???)
      case _ => base
    }
  }

  private def typeApplication(): Type = {
    val fn = peek(prim)
    val argsBuilder = List.newBuilder[Type]
    while !isAtEnd && matches(primStarts*) do argsBuilder += peek(prim)
    argsBuilder.result() match {
      case Nil => fn
      case xs  => Type.App(fn, xs)(using ???)
    }
  }

  val primStarts: Seq[Token.Kind] = Seq(Token.OpenParen, Token.LowerName, Token.UpperName)

  val prim: PartialFunction[Token.Kind, Type] = {
    case Token.OpenParen =>
      val openParen = advance()
      peek {
        case Token.CloseParen => Type.Unit()(using ???)
        case _ =>
          val elems = List.newBuilder[Type]
          elems += tpe()
          while !isAtEnd && matches(Token.Comma) do
            consume(Token.Comma)
            elems += tpe()
          consume(Token.CloseParen)

          elems.result() match
            case List(x) => x
            case xs      => Type.Tuple(xs)(using ???)
      }

    case Token.LowerName =>
      val name = advance()
      Type.Var(name.literal)(using ???)

    case Token.UpperName =>
      val name = advance()
      Type.Named(name.literal)(using ???)
  }
}

object TypeParser {}
