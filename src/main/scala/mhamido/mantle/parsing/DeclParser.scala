package mhamido.mantle.parsing

trait DeclParser extends Parser {
  self: ExprParser & TypeParser & PatternParser =>
  def decl(terminators: Token.Kind*): Decl = expect {
    case Token.Fun =>
      val fun = summon[Token].pos
      val name = consume(Token.LowerName).get
      val typarams = typeParams()
      val params = patterns(Token.Colon, Token.Eql)
      // val params = Seq()
      val returnType = peek {
        case Token.Colon =>
          val colon = advance()
          val ret = tpe(Token.Eql +: terminators)
          ret
        case _ => Type.Unit()(using pos <> pos)
      }
      consume(Token.Eql)
      val body = expr()
      Decl.Fun(name.literal, typarams, params, returnType, body)(using fun <> body.info)

    case Token.Val =>
      val token = summon[Token]
      val patt = pattern(Token.Eql)
      consume(Token.Eql)
      val body = expr()
      Decl.Val(patt, body)(using token.pos <> body.info)

    case Token.Mutual =>
      val open = summon[Token].pos
      consume(Token.OpenBrace)
      val defs = decls(Token.CloseBrace)
      consume(Token.CloseBrace)
      Decl.Mutual(defs)(using open <> pos)

    case Token.Type =>
      val start = summon[Token].pos
      val name = consume(Token.UpperName).get.literal
      val params = typeParams()
      consume(Token.Eql)
      val ty = tpe(DeclParser.Start)
      Decl.TypeAlias(name, params, ty)(using start <> ty.info)

    case Token.DataType =>
      val start = summon[Token].pos
      val name = consume(Token.UpperName).get.literal
      val tpe = typeParams()
      consume(Token.Eql)
      val body: Seq[(Name, Seq[Name])] = {
        ???
      }
    
      Decl.TypeDef(name, tpe, body)(using start <> pos)
  }

  def decls(terminator: Token.Kind): Seq[Decl] =
    val defs = List.newBuilder[Decl]
    while !isAtEnd && !matches(terminator) do defs += decl()
    defs.result()

  def typeParams(): Seq[Name] = peek {
    case Token.OpenBracket =>
      advance()
      val params = List.newBuilder[Name]
      consume(Token.LowerName).foreach(params += _.literal)

      while !isAtEnd && matches(Token.Comma) do
        consume(Token.Comma)
        consume(Token.LowerName).foreach(params += _.literal)

      consume(Token.CloseBracket)
      params.result()
    case _ => Seq.empty
  }
}

object DeclParser {
  val Start: Seq[Token.Kind] = Seq(
    Token.Fun,
    Token.Val,
    Token.Mutual,
    Token.Type,
    Token.DataType
  )
}
