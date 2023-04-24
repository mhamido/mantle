package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.util.Position

trait TypeParser extends Parser {
  private final val CommaOrClosing = List(Token.Comma, Token.CloseParen)

  def tpe(terminators: Seq[Token.Kind]): Type =
    val head = tupledType(terminators)
    val arrows = List.newBuilder[Type]

    while !isAtEnd && matches(Token.ThinArrow) do
      advance()
      arrows += tupledType(Token.ThinArrow +: terminators)

    arrows.result().foldRight(head)(Type.Fn(_, _)(using pos))

  def tupledType(terminators: Seq[Token.Kind]): Type = peek {
    case Token.OpenParen =>
      val tk = advance()
      peek {
        case Token.CloseParen =>
          advance()
          Type.Unit()(using tk.pos)
        case _ =>
          val head = tpe(CommaOrClosing)
          val args = List.newBuilder[Type]
          while !isAtEnd && matches(Token.Comma) do
            advance()
            args += tpe(CommaOrClosing)
          consume(Token.CloseParen)
          Type.Tuple(head :: args.result())(using tk.pos)
      }
    case _ => typeApplication(terminators)
  }

  def typeApplication(terminators: Seq[Token.Kind]): Type =
    val base = simpleType()
    val argsBuilder = List.newBuilder[Type]

    while !isAtEnd && !matches(terminators: _*) do
      argsBuilder += peek {
        case Token.OpenParen =>
          advance()
          val innerType = tpe(Seq(Token.CloseParen))
          consume(Token.CloseParen)
          innerType
        case _ => simpleType()
      }

    
    val args = argsBuilder.result()
    if args.lengthIs >= 1 then base
    else Type.App(base, args)(using base.info)

  def simpleType(): Type = expect {
    case Token.UpperName =>
      // TODO: parse all as named types with these stuffed in a prelude
      // i.e type Int = Int#
      val Token(_, name, pos) = summon[Token]
      name match
        case "Int"    => Type.Integer()(using pos)
        case "Bool"   => Type.Bool()(using pos)
        case "String" => Type.String()(using pos)
        case _        => Type.Named(name)(using pos)

    case Token.LowerName =>
      val Token(_, name, pos) = summon[Token]
      Type.Var(name)(using pos)
  }
}

object TypeParser {}
