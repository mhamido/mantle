package mhamido.mantle.syntax.parser

import mhamido.mantle.syntax.Type
import mhamido.mantle.Token
import mhamido.mantle.syntax.Name

trait TypeParser extends ParserCommons {
  private final val CommaOrClosing = List(Token.Comma, Token.CloseParen)

  def tpe(terminators: Seq[Token.Kind]): Type =
    val head = tupledType(terminators)
    val arrows = List.newBuilder[Type]

    while !isAtEnd && matches(Token.ThinArrow) do
      advance()
      arrows += tupledType(Token.ThinArrow +: terminators)

    arrows.result().foldRight(head)(Type.Arrow(_, _))

  def tupledType(terminators: Seq[Token.Kind]): Type = peek {
    case Token.OpenParen =>
      advance()
      peek {
        case Token.CloseParen =>
          advance()
          Type.Unit
        case _ =>
          val head = tpe(CommaOrClosing)
          val args = List.newBuilder[Type]
          while !isAtEnd && matches(Token.Comma) do
            advance()
            args += tpe(CommaOrClosing)
          consume(Token.CloseParen)
          Type.Tuple(head :: args.result())

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

    argsBuilder.result().foldLeft(base)(Type.Apply(_, _))

  def simpleType(): Type = expect {
    case Token.UpperName =>
      val name = Name.Constructor(summon[Token].literal)
      Type.Primitives.getOrElse(name, Type.Named(name))

    case Token.LowerName | Token.PrimedName =>
      val name = Name.TypeVar(summon[Token].literal)
      Type.TypeVar(name)
  }
}
