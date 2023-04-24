package mhamido.mantle.parsing

trait PatternParser extends Parser { self: TypeParser =>
  def patterns(terminators: Token.Kind*): Seq[Pattern] =
    val patts = List.newBuilder[Pattern]
    while !isAtEnd && !matches(terminators*) do patts += simplePattern()
    patts.result()

  def pattern(terminators: Token.Kind*): Pattern =
    val patt = ascriptionPattern((Token.As +: terminators)*)
    peek {
      case Token.As =>
        advance()
        consume(Token.LowerName)
          .fold(patt)(x => Pattern.Alias(patt, x.literal)(using pos))
      case _ => patt
    }

  def ascriptionPattern(terminators: Token.Kind*): Pattern =
    val patt = applicationPattern(terminators: _*)
    peek {
      case Token.Colon =>
        advance()
        val typ = tpe(terminators)
        Pattern.Ascribe(patt, typ)(using pos)
      case _ => patt
    }

  def applicationPattern(terminators: Token.Kind*): Pattern = peek {
    case Token.UpperName =>
      val constructor = advance()
      val arguments = List.newBuilder[Pattern]

      while !isAtEnd && !matches(terminators: _*) do
        arguments += simplePattern()

      Pattern.Constructor(constructor.literal, arguments.result())(using
        constructor.pos
      )

    case _ =>
      simplePattern()
  }

  def simplePattern(): Pattern = expect {
    case Token.Int =>
      Pattern.Int(summon[Token].literal.toInt)(using summon[Token].pos)
    case Token.String =>
      Pattern.String(summon[Token].literal)(using summon[Token].pos)
    case Token.LowerName =>
      summon[Token].literal match
        case "_" =>
          Pattern.Wildcard()(using
            summon[Token].pos
          ) // TODO: make _ a token on it's own.
        case "true"  => Pattern.True()(using summon[Token].pos)
        case "false" => Pattern.False()(using summon[Token].pos)
        case name    => Pattern.Var(name)(using summon[Token].pos)

    case Token.UpperName =>
      Pattern.Constructor(summon[Token].literal, Nil)(using summon[Token].pos)

    case Token.OpenParen =>
      val patt: Pattern = peek {
        case Token.CloseParen => Pattern.Unit()(using pos)
        case _ =>
          val subpatterns = List.newBuilder[Pattern]
          subpatterns += pattern(Token.Comma, Token.CloseParen)

          while !isAtEnd && matches(Token.Comma) do
            advance()
            subpatterns += pattern(Token.Comma, Token.CloseParen)

          // A pattern like `(p)` should just be equivalent to `p`.
          subpatterns.result() match
            case List(patt) => patt
            case patterns   => Pattern.Tuple(patterns)(using patterns(0).info)
      }

      consume(Token.CloseParen)
      patt
  }
}
