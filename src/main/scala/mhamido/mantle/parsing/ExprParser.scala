package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.Operator

trait ExprParser extends Parser {
  self: DeclParser & TypeParser & PatternParser =>
  def expr(terminator: Token.Kind*): Expr = peek {
    case Token.Let =>
      advance()
      val defs = Decl.Mutual(decls(Token.In))(using pos)
      consume(Token.In)
      val body = expr(terminator*)
      Expr.Let(Seq(defs), body)(using pos)

    case Token.If =>
      advance()
      val cond = condExpr()
      consume(Token.Then)
      val thenp = expr(Token.Else)
      consume(Token.Else)
      val elsep = expr(terminator*)
      Expr.If(cond, thenp, elsep)(using pos)

    case Token.While =>
      advance()
      val cond = condExpr()
      consume(Token.Do)
      val body = expr(terminator*)
      Expr.While(cond, body)(using pos)

    case Token.Fn =>
      advance()
      val pos = summon[Token].pos
      val params = patterns(Token.ThickArrow)
      consume(Token.ThickArrow)
      val body = expr(terminator*)
      Expr.Fn(params, body)(using pos)

    case Token.For =>
      val init = pattern(Token.LeftArrow)
      consume(Token.LeftArrow)
      val start = condExpr(Token.To)
      consume(Token.To)
      val end = condExpr(Token.Do)
      consume(Token.Do)
      val body = expr(terminator*)
      Expr.For(init, start, end, body)(using summon[Token].pos)

    case Token.Match =>
      val scrutnee = condExpr(Token.With)
      consume(Token.With)
      consume(Token.OpenBrace)
      val cases = matchArms()
      consume(Token.CloseBrace)
      Expr.Case(scrutnee, cases)(using pos)

    case _ => condExpr(terminator*)
  }

  private def condExpr(terminators: Token.Kind*): Expr = unary(terminators*)
  private def matchArms(): Seq[MatchArm] = ???

  private def unary(terminator: Token.Kind*): Expr = peek {
    case Token.Not =>
      val tk = advance()
      val expr = application(terminator*)
      Expr.Not(expr)(using tk.pos)

    case Token.Sub =>
      val tk = advance()
      val expr = application(terminator*)
      Expr.Negate(expr)(using tk.pos)

    case _ => application(terminator*)
  }

  private def application(terminators: Token.Kind*): Expr =
    val fn = atomic()
    val args = List.newBuilder[Expr]

    while !isAtEnd && !matches(terminators*) && matches(ExprParser.Start*) do
      args += atomic()
    args.result().foldLeft(fn)(Expr.Apply(_, _)(using fn.info))

  private def atomic(): Expr = expect {
    case Token.Int    => Expr.Int(summon[Token].literal.toInt)(using pos)
    case Token.String => Expr.String(summon[Token].literal)(using pos)
    case Token.LowerName if summon[Token].literal == "true" =>
      Expr.True()(using pos)
    case Token.LowerName if summon[Token].literal == "false" =>
      Expr.False()(using pos)
    case Token.LowerName | Token.UpperName =>
      Expr.Var(summon[Token].literal)(using pos)
    case Token.OpenParen =>
      val result = expr(Token.CloseParen, Token.Comma)
      val elems = peek {
        case Token.Comma =>
          val elemsbuilder = List.newBuilder[Expr]
          elemsbuilder += result
          while !isAtEnd && matches(Token.Comma) do
            advance()
            elemsbuilder += expr(Token.Comma, Token.CloseParen)
          val elems = elemsbuilder.result()
          Expr.Tuple(elems)(using result.info)
        case _ => result
      }
      consume(Token.CloseParen)
      elems
  }

  def level(
      tokens: (Token.Kind, Operator)*
  )(higherPrecExpr: () => Expr): Expr = {
    var result = higherPrecExpr()
    val operators = tokens.map(_._1)
    val operatorMap = tokens.toMap
    while !isAtEnd && matches(operators*) do
      val token = advance()
      val oper = operatorMap(token.kind)
      result = Expr.Bin(oper, result, higherPrecExpr())(using result.info)
    result
  }

  // Table of operator precedences in increasing order.
  def condExpr(): Expr = level(Token.LogicalOr -> Operator.LogicalOr)(andExpr)
  def andExpr(): Expr = level(Token.LogicalAnd -> Operator.LogicalAnd)(eqlExpr)
  def eqlExpr(): Expr = level(
    Token.Eql -> Operator.Eql,
    Token.NotEql -> Operator.NotEql
  )(compareExpr)

  def compareExpr(): Expr = level(
    Token.GreaterThan -> Operator.GreaterThan,
    Token.LessThan -> Operator.LessThan,
    Token.LessThanOrEql -> Operator.LessThanOrEql,
    Token.GreaterThanOrEql -> Operator.GreaterThanOrEql
  )(term)

  def term(): Expr = level(
    Token.Add -> Operator.Add,
    Token.Sub -> Operator.Sub
  )(factor)

  def factor(): Expr = level(
    Token.Mul -> Operator.Mul,
    Token.Div -> Operator.Div
  )(() => application(Operator.tokenKinds*))
}

object ExprParser {
  val Start: Seq[Token.Kind] = Seq[Token.Kind](
    Token.Int,
    Token.String,
    Token.LowerName,
    Token.UpperName,
    Token.OpenParen
  )
}
