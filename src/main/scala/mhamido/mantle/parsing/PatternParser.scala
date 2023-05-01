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
          .fold(patt)(x =>
            Pattern.Alias(patt, x.literal)(using patt.info <> pos)
          )
      case _ => patt
    }

  def ascriptionPattern(terminators: Token.Kind*): Pattern =
    val patt = applicationPattern(terminators: _*)
    peek {
      case Token.Colon =>
        advance()
        val typ = tpe(terminators)
        Pattern.Ascribe(patt, typ)(using patt.info <> pos)
      case _ => patt
    }

  def applicationPattern(terminators: Token.Kind*): Pattern = peek {
    case Token.UpperName =>
      val constructor = advance()
      val arguments = List.newBuilder[Pattern]

      while !isAtEnd && !matches(terminators: _*) do
        arguments += simplePattern()

      Pattern.Constructor(constructor.literal, arguments.result())(using
        constructor.pos <> pos
      )

    case _ =>
      simplePattern()
  }

  def simplePattern(): Pattern = expect {
    case Token.Int =>
      val token = summon[Token]
      Pattern.Int(token.literal.toInt)(using token.pos <> pos)
    case Token.String =>
      val token = summon[Token]
      Pattern.String(token.literal)(using token.pos <> pos)
    case Token.LowerName =>
      val token = summon[Token]
      token.literal match
        case "_" =>
          Pattern.Wildcard()(using token.pos <> pos)
          // TODO: make _ a token on it's own.
        case "true"  => Pattern.True()(using summon[Token].pos <> pos)
        case "false" => Pattern.False()(using summon[Token].pos <> pos)
        case name    => Pattern.Var(name)(using summon[Token].pos <> pos)

    case Token.UpperName =>
      Pattern.Constructor(summon[Token].literal, Nil)(using summon[Token].pos <> pos)

    case Token.OpenParen =>
      val openParen = summon[Token]
      val patt: Pattern = peek {
        case Token.CloseParen => Pattern.Unit()(using openParen.pos <> pos)
        case _ =>
          val subpatterns = List.newBuilder[Pattern]
          subpatterns += pattern(Token.Comma, Token.CloseParen)

          while !isAtEnd && matches(Token.Comma) do
            advance()
            subpatterns += pattern(Token.Comma, Token.CloseParen)

          // A pattern like `(p)` should just be equivalent to `p`.
          subpatterns.result() match
            case List(patt) => patt
            case patterns   => Pattern.Tuple(patterns)(using openParen.pos <> pos)
      }

      consume(Token.CloseParen)
      patt
  }
}
