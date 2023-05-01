package mhamido.mantle.parsing

import mhamido.mantle.syntax
import mhamido.mantle.Operator

trait ExprParser extends Parser {
  self: DeclParser & TypeParser & PatternParser =>

  type Prefix = Token => Expr
  type Infix = (Expr, Token) => Expr

  private val prefixes: PartialFunction[Token.Kind, Prefix] = {
    case Token.Not => { token =>
      val rhs = expr(ExprParser.HighestPrec)
      Expr.Not(rhs)(using token.pos <> rhs.info)
    }
    case Token.Sub => { token =>
      val rhs = expr(ExprParser.HighestPrec)
      Expr.Negate(rhs)(using token.pos <> rhs.info)
    }
    case Token.If => { token =>
      val cond = expr()
      consume(Token.Then)
      val thenp = expr() // (Token.Else)
      consume(Token.Else)
      val elsep = expr()
      Expr.If(cond, thenp, elsep)(using token.pos <> elsep.info)
    }
    case Token.Let => { token =>
      val defs = Decl.Mutual(decls(Token.In))(using token.pos <> pos)
      consume(Token.In)
      val body = expr()
      Expr.Let(Seq(defs), body)(using token.pos <> pos)
    }
    case Token.While => { token =>
      val cond = expr()
      consume(Token.Do)
      val body = expr()
      Expr.While(cond, body)(using token.pos <> pos)
    }
    case Token.Fn => { token =>
      val params = patterns(Token.ThickArrow)
      consume(Token.ThickArrow)
      val body = expr()
      Expr.Fn(params, body)(using token.pos <> pos)
    }
    case Token.For => { token =>
      val init = pattern(Token.LeftArrow)
      consume(Token.LeftArrow)
      val start = expr() // condExpr(Token.To)
      consume(Token.To)
      val end = expr() // condExpr(Token.Do)
      consume(Token.Do)
      val body = expr()
      Expr.For(init, start, end, body)(using token.pos <> pos)
    }
    case Token.Case => { token =>
      val scrutnee = expr() // condExpr(Token.With)
      consume(Token.Of)
      consume(Token.OpenBrace)
      val cases = matchArms()
      consume(Token.CloseBrace)
      Expr.Case(scrutnee, cases)(using token.pos <> pos)
    }

    case kind if ExprParser.PrimStart contains kind => { token =>
      val fn = prim(token)
      val args = List.newBuilder[Expr]
      while !isAtEnd && matches(ExprParser.PrimStart*) do
        args += prim(advance())
      val argsResult = args.result()
      argsResult.foldLeft(fn)(Expr.Apply(_, _)(using fn.info.from <> pos))
    }
  }

  private val leftAssoc: Infix = (lhs, op) => {
    Expr.Bin(Operator(op.kind), lhs, expr(precedence(op.kind)))(using lhs.info)
  }

  private def prim(token: Token): Expr = token.kind match {
    case Token.Int =>
      Expr.Int(token.literal.toInt)(using token.pos <> pos)

    case Token.String =>
      Expr.String(token.literal)(using token.pos <> pos)

    case Token.LowerName =>
      if (token.literal == "true")
        Expr.True()(using token.pos <> pos)
      else if (token.literal == "false")
        Expr.False()(using token.pos <> pos)
      else Expr.Var(token.literal)(using token.pos <> pos)

    case Token.UpperName =>
      Expr.Var(token.literal)(using token.pos <> pos)

    case Token.OpenParen if peek.kind == Token.CloseParen =>
      advance()
      Expr.Unit()(using token.pos <> pos)

    case Token.OpenParen =>
      val result = expr() // (Token.CloseParen, Token.Comma)
      val elems = peek {
        case Token.Comma =>
          val elemsbuilder = List.newBuilder[Expr]
          elemsbuilder += result
          while !isAtEnd && matches(Token.Comma) do
            advance()
            elemsbuilder += expr() // (Token.Comma, Token.CloseParen)
          val elems = elemsbuilder.result()
          Expr.Tuple(elems)(using token.pos <> pos)
        case _ => result
      }
      consume(Token.CloseParen)
      elems

    case tpe =>
      reporter.fatalError(
        Parser.Unexpected(ExprParser.PrimStart, Some(token)),
        token.pos
      )
  }

  private val infixes = ExprParser.Operators.flatten.map(_ -> leftAssoc).toMap

  private def precedence(x: Token.Kind): Int =
    ExprParser.Operators.reverse
      .indexWhere(_.contains(x))

  private def unary(): (Expr, Token) =
    prefixes.lift(peek.kind) match {
      case None =>
        reporter.fatalError(
          Parser.Unexpected(
            Token.Kind.values.toSeq.filter(prefixes.isDefinedAt(_)),
            if !isAtEnd then Some(peek) else None
          )
        )

      case Some(prefixParser) =>
        val token = advance()
        val result = prefixParser(token)
        (result, token)
    }

  def expr(prec: Int = 0): Expr = {
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

  private def matchArm(): MatchArm = {
    val pat = pattern(Token.ThinArrow, Token.If)
    val guard = peek {
      case Token.If =>
        advance()
        Some(expr())
      case _ => None
    }

    consume(Token.ThinArrow)
    val body = expr()
    MatchArm(pat, body, guard)
  }

  private def matchArms(): Seq[MatchArm] = peek {
    case Token.CloseBrace => Seq.empty
    case _ =>
      val cases = List.newBuilder[MatchArm]
      cases += matchArm()

      while !isAtEnd && !matches(Token.CloseBrace) do
        consume(Token.Semi)
        cases += matchArm()

      // TODO: fix trailing semi in cases
      peek {
        case Token.Semi => advance()
        case _          =>
      }

      cases.result()
  }

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
    Seq(Token.LogicalOr)
  )

  val HighestPrec = Operators.map(_.length).sum + 1
  val PrimStart = Seq(
    Token.Int,
    Token.String,
    Token.LowerName,
    Token.UpperName,
    Token.OpenParen
  )
}
