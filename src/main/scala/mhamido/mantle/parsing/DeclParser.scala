package mhamido.mantle.parsing

trait DeclParser extends Parser {
  self: ExprParser & TypeParser & PatternParser =>

  def decl(terminators: Token.Kind*): Decl = expect {
    case Token.Fun =>
      val fun   = summon[Token]
      val group = funClause(fun) :: andClauses()
      Decl.FunGroup(group)(using fun.pos <> pos)

    case Token.Val =>
      val vl     = summon[Token]
      val binder = basicPattern()
      val explicitType = peek {
        case Token.Colon =>
          val colon = advance()
          Some(tpe())
        case _ => None
      }
      consume(Token.Eql)
      val body = expr()
      Decl.Val(binder, explicitType, body)(using vl.pos <> pos)

    case Token.Type =>
      val start          = summon[Token]
      val name           = consume(Token.UpperName).get.literal
      val (_, params, _) = uncurriedTypeParams()
      consume(Token.Eql)
      val body = tpe()
      Decl.Alias(name, params, body)(using start.pos <> pos)

    case Token.DataType =>
      val start          = summon[Token]
      val name           = consume(Token.UpperName).get.literal
      val (_, params, _) = uncurriedTypeParams()
      consume(Token.Eql)
      ???
  }

  def decls(terminators: Token.Kind*): List[Decl] = peek {
    case end if terminators contains end => Nil
    case declStart if DeclParser.Start contains declStart =>
      decl(terminators*) :: decls(terminators*)
  }

  private def funClause(start: Token): Decl.Fun = expect {
    case Token.LowerName =>
      val name = summon[Token].literal
      val paramList = params()
      consume(Token.Colon)
      val retType = tpe()
      consume(Token.Eql)
      val body = expr()
      Decl.Fun(name, paramList, retType, body)(using
        start.pos <> body.info
      )
  }

  private def andClauses(): List[Decl.Fun] = peek {
    case Token.And =>
      val andToken = advance()
      val clause   = funClause(andToken)
      clause :: andClauses()
    case _ =>
      Nil
  }

  def params(): Seq[Param] = {
    def param(): Param = expect {
      case Token.OpenBracket =>
        val name = consume(Token.PrimedName).get
        consume(Token.CloseBracket)
        Param.Type(name.literal)(using name.pos <> pos)
      case Token.OpenParen =>
        val openParen = summon[Token]
        expect {
          case Token.LowerName =>
            val name = consume(Token.LowerName).get
            consume(Token.Colon)
            val annotatedType = tpe()
            consume(Token.CloseParen)
            Param.Value(name.literal, annotatedType)(using name.pos <> pos)
          case Token.CloseParen =>
            val pos = openParen.pos <> this.pos
            Param.Value("Unit", Type.Unit()(using pos))(using pos)
        }

    }

    def loop(): List[Param] =
      peek {
        case Token.OpenBracket | Token.OpenParen => param() :: loop()
        case _                                   => Nil
      }

    param() :: loop()
  }
  private def uncurriedTypeParams() =
    between(Token.OpenBracket, Token.Comma, Token.CloseBracket) { () =>
      consume(Token.PrimedName).get.literal
    }
}

object DeclParser {
  val Start: Seq[Token.Kind] = Seq(
    Token.Fun,
    Token.Val,
    Token.Type,
    Token.DataType
  )
}
