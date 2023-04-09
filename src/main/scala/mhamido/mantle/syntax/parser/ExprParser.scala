package mhamido.mantle.syntax.parser

import mhamido.mantle.syntax.Expr
import mhamido.mantle.Token
import mhamido.mantle.syntax.Pattern
import mhamido.mantle.syntax.Operator

trait ExprParser extends ParserCommons {
  self: DeclParser & PatternParser & TypeParser =>

  def expr(): Expr = expect(unaryExpr()) {
    case Token.Let =>
      val defs = decls(Token.In)
      consume(Token.In)
      val body = expr()
      Expr.Let(defs, body)

    case Token.If =>
      val cond = condExpr()
      consume(Token.Then)
      val thenp = expr()
      consume(Token.Else)
      val elsep = expr()

      Expr.If(cond, thenp, elsep)

    case Token.While =>
      val cond = condExpr()
      consume(Token.Do)
      val body = expr()
      Expr.While(cond, body)

    // case Token.For =>
    //   val init = condExpr()
    //   consume(Token.LeftArrow)
    //   ???

    case Token.Match =>
      val scrutnee = condExpr()
      consume(Token.With)
      consume(Token.OpenBrace)
      val cases = matchArms()
      consume(Token.CloseBrace)
      Expr.Case(scrutnee, cases)
  }

  private def matchArms(): Seq[Expr.MatchArm] =
    def matchArm(): Expr.MatchArm =
      val patt = pattern(Token.If, Token.ThinArrow)
      val guard = peek {
        case Token.If =>
          advance()
          Some(expr())
        case _ => None
      }
      consume(Token.ThinArrow)
      val body = expr()
      Expr.MatchArm(patt, body, guard)

    val arms = List.newBuilder[Expr.MatchArm]

    if !matches(Token.CloseBrace) then arms += matchArm()

    while !isAtEnd && !matches(Token.CloseBrace) do
      consume(Token.Semi)
      arms += matchArm()

    arms.result()
  
  def condExpr(): Expr = level(condExpr, Token.LogicalOr)
  def andExpr(): Expr = level(andExpr, Token.LogicalAnd)
  def eqlExpr(): Expr = level(eqlExpr, Token.Eql, Token.NotEql)
  def compareExpr(): Expr = level(compareExpr, Token.LessThan, Token.GreaterThan, Token.LessThanOrEql, Token.GreaterThanOrEql)
  def term(): Expr = level(term, Token.Add, Token.Sub)
  def factor(): Expr = level(factor, Token.Mul, Token.Div)
  def application(): Expr = level(application, )
  def selectExpr(): Expr = level(selectExpr, )
  def unaryExpr(): Expr = peek {
    case Token.Not =>
      advance()
      Expr.Not(unaryExpr())
    case Token.Sub =>
      advance()
      Expr.Negate(unaryExpr())
    case _ =>
      atomicExpr()
  }

  private def atomicExpr(): Expr = expect {
    case Token.Int    => Expr.Int(summon[Token].literal.toInt)
    case Token.String => Expr.String(summon[Token].literal)
    case Token.LowerName if summon[Token].literal == "true"  => Expr.True
    case Token.LowerName if summon[Token].literal == "false" => Expr.False
    case Token.LowerName => Expr.Var(summon[Token].toName)
    case Token.UpperName => Expr.Con(summon[Token].toName)
    case Token.OpenParen =>
      val result = expr()
      consume(Token.CloseParen)
      result
  }

  private def level(
      higher: () => Expr,
      operators: Token.Kind*
  ): Expr =
    var result = higher()
    while !isAtEnd && matches(operators: _*) do
      val token = advance()
      val op = Operator(token.kind)
      result = Expr.Bin(op, result, higher())
    result

}
