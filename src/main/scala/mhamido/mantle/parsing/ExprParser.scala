package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.Operator

trait ExprParser extends Parser {
  self: DeclParser & TypeParser & PatternParser =>

  type Prefix = Token => Expr
  type Infix = (Expr, Token) => Expr

  val prefixes = Map[Token.Kind, Prefix](
    Token.Not -> { token =>
      val rhs = expr(ExprParser.HighestPrec)
      Expr.Not(rhs)(using token.pos)
    },
    Token.Sub -> { token =>
      val rhs = expr(ExprParser.HighestPrec)
      Expr.Negate(rhs)(using token.pos)
    },
    Token.If -> { token =>
      val cond = expr()
      consume(Token.Then)
      val thenp = expr() // (Token.Else)
      consume(Token.Else)
      val elsep = expr()
      Expr.If(cond, thenp, elsep)(using token.pos)
    },
    Token.Let -> { token =>
      val defs = Decl.Mutual(decls(Token.In))(using token.pos)
      consume(Token.In)
      val body = expr()
      Expr.Let(Seq(defs), body)(using token.pos)
    },
    Token.While -> { token =>
      val cond = expr()
      consume(Token.Do)
      val body = expr()
      Expr.While(cond, body)(using token.pos)
    },
    Token.Fn -> { token =>
      val params = patterns(Token.ThickArrow)
      consume(Token.ThickArrow)
      val body = expr()
      Expr.Fn(params, body)(using token.pos)
    },
    Token.For -> { token =>
      val init = pattern(Token.LeftArrow)
      consume(Token.LeftArrow)
      val start = expr() // condExpr(Token.To)
      consume(Token.To)
      val end = expr() // condExpr(Token.Do)
      consume(Token.Do)
      val body = expr()
      Expr.For(init, start, end, body)(using token.pos)
    },
    Token.Match -> { token =>
      val scrutnee = expr() // condExpr(Token.With)
      consume(Token.With)
      consume(Token.OpenBrace)
      val cases = matchArms()
      consume(Token.CloseBrace)
      Expr.Case(scrutnee, cases)(using token.pos)
    },
    Token.Int -> { token => Expr.Int(token.literal.toInt)(using token.pos) },
    Token.String -> { token => Expr.String(token.literal)(using token.pos) },
    Token.LowerName -> { token =>
      if (token.literal == "true")
        Expr.True()(using token.pos)
      else if (token.literal == "false")
        Expr.False()(using token.pos)
      else Expr.Var(token.literal)(using token.pos)
    },
    Token.UpperName -> { token =>
      Expr.Var(token.literal)(using token.pos)
    },
    Token.OpenParen -> { token =>
      val result = expr() // (Token.CloseParen, Token.Comma)
      val elems = peek {
        case Token.Comma =>
          val elemsbuilder = List.newBuilder[Expr]
          elemsbuilder += result
          while !isAtEnd && matches(Token.Comma) do
            advance()
            elemsbuilder += expr() // (Token.Comma, Token.CloseParen)
          val elems = elemsbuilder.result()
          Expr.Tuple(elems)(using token.pos)
        case _ => result
      }
      consume(Token.CloseParen)
      elems
    }
  )

  val leftAssoc: Infix = (lhs, op) => {
    Expr.Bin(Operator(op.kind), lhs, expr(precedence(op.kind)))(using lhs.info)
  }

  // val infixes: Map[Token.Kind, Infix] = List(
  //   Token.Mul,
  //   Token.Div,
  //   Token.Add,
  //   Token.Sub,
  //   Token.LessThan,
  //   Token.GreaterThan,
  //   Token.Eql,
  //   Token.NotEql,
  //   Token.LogicalOr,
  //   Token.LogicalAnd,
  //   Token.Semi
  // ).map(_ -> leftAssoc).toMap
  val infixes = ExprParser.Operators.flatten.map(_ -> leftAssoc).toMap

  def precedence(x: Token.Kind): Int =
    ExprParser.Operators.reverse
      .indexWhere(_.contains(x))

  def expr(prec: Int = 0): Expr = {
    def unary(): (Expr, Token) = prefixes.get(peek.kind) match {
      case None =>
        reporter.fatalError(
          Parser.Unexpected(
            prefixes.keys.toSeq,
            if !isAtEnd then Some(peek) else None
          )
        )
      case Some(prefixParser) =>
        val token = advance()
        (prefixParser(token), token)
    }

    def nextPrec: Int = {
      infixes.get(peek.kind).fold(0)(_ => precedence(peek.kind))
    }

    def loop(left: Expr, token: Token): Expr = {
      // pprint.pprintln((left, token), showFieldNames = false, height = 1)
      if (prec < nextPrec) {
        val currToken = advance()
        val infixParser = infixes(currToken.kind)
        val currLeft = infixParser(left, currToken)
        loop(currLeft, currToken)
      } else {
        left
      }
    }

    val (expr, token) = unary()
    loop(expr, token)
  }
  // given Ordering[Token.Kind] with {}

  private def matchArms(): Seq[MatchArm] = ???

  private def fnApplication(left: Expr): Expr = ???

  // private def application(terminators: Token.Kind*): Expr =
  //   val fn = atomic()
  //   val args = List.newBuilder[Expr]

  //   while !isAtEnd && !matches(terminators*) && matches(ExprParser.Start*) do
  //     args += atomic()
  //   args.result().foldLeft(fn)(Expr.Apply(_, _)(using fn.info))
}

object ExprParser {
  val Operators: Seq[Seq[Token.Kind]] = Seq(
    Seq(Token.Mul, Token.Div),
    Seq(Token.Add, Token.Sub),
    Seq(
      Token.LessThan,
      Token.LessThanOrEql,
      Token.GreaterThan,
      Token.GreaterThanOrEql
    ),
    Seq(Token.Eql, Token.NotEql),
    Seq(Token.LogicalAnd),
    Seq(Token.LogicalOr),
    Seq(Token.Semi)
  )

  val HighestPrec = Operators.map(_.length).sum + 1
}
