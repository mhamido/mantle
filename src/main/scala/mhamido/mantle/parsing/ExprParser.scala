package mhamido.mantle.parsing

import mhamido.mantle.util.Span
import org.parboiled2.*
import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either
import mhamido.mantle.Operator

trait ExprParser extends BaseParser:
  self: TypeParser & DeclParser & PatternParser =>

  private def TupledExpr: Rule1[syntax.Expr] =
    def unit = rule:
      push(cursor) ~ "(" ~ ")" ~> { (start) =>
        syntax.Expr.Unit()(using Span(start, cursor))
      }

    def nonEmptyTuple = rule:
      push(cursor) ~ "(" ~ (Expr + ",") ~ ")" ~> { (start, elems) =>
        if elems.lengthIs == 1 then elems.head
        else syntax.Expr.Tuple(elems)(using Span(start, cursor))
      }
    rule(unit | nonEmptyTuple)

  private def AtomicExpr: Rule1[syntax.Expr] =
    def int = rule:
      push(cursor) ~ Integer ~> { (start, it) =>
        syntax.Expr.Int(it)(using Span(start, cursor))
      }

    def bool = rule:
      push(cursor) ~ capture("true" | "false") ~> { (start, lit) =>
        given Span = Span(start, cursor)
        lit.strip() match
          case "true"  => syntax.Expr.True()
          case "false" => syntax.Expr.False()
      }

    def varname = rule:
      push(cursor) ~ (LowerName | UpperName) ~> { (start, name) =>
        syntax.Expr.Var(name)(using Span(start, cursor))
      }

    def string = rule:
      push(cursor) ~ StringLiteral ~> { (start, str) =>
        syntax.Expr.String(str)(using Span(start, cursor))
      }

    def char = rule:
      push(cursor) ~ CharLiteral ~> { (start, chr) =>
        syntax.Expr.Chr(chr.head)(using Span(start, cursor))
      }

    rule(TupledExpr | int | bool | string | char | varname)

  def Expr: Rule1[syntax.Expr] =
    def fnExpr: Rule1[syntax.Expr] = rule:
      push(cursor) ~ "fn" ~ Param.+ ~ "=>" ~ Expr ~> { (start, params, body) =>
        syntax.Expr.Fn(params, body)(using Span(start, cursor))
      }

    def ifExpr: Rule1[syntax.Expr] = rule:
      push(cursor) ~ "if" ~ Expr ~ "then" ~ Expr ~ "else" ~ Expr ~> {
        (start, cond, thenp, elsep) =>
          syntax.Expr.If(cond, thenp, elsep)(using Span(start, cursor))
      }
    def matchExpr: Rule1[syntax.Expr] = rule:
      push(
        cursor
      ) ~ "match" ~ Expr1 ~ "with" ~ "|".? ~ (MatchArm * "|")
        .named("alternatives") ~ "end" ~> { (start, scrutnee, arms) =>
        syntax.Expr.Match(scrutnee, arms)(using Span(start, cursor))
      }
    def letExpr: Rule1[syntax.Expr] = rule:
      push(cursor) ~ "let" ~ Decl.+ ~ "in" ~ Expr ~> { (start, decls, body) =>
        syntax.Expr.Let(decls, body)(using Span(start, cursor))
      }

    def ascriptionExpr: Rule1[syntax.Expr] = rule:
      Expr1 ~ (":" ~ Type).? ~> { (expr, tpe) =>
        tpe.fold(expr)(tpe =>
          syntax.Expr.Ascribe(expr, tpe)(using expr.info <> tpe.info)
        )
      }
    rule(fnExpr | ifExpr | letExpr | matchExpr | ascriptionExpr)

  inline def InfixLeft(expr: => Rule1[syntax.Expr])(
      ops: (String, Operator)*
  ): Rule1[syntax.Expr] =
    InfixLeft(expr, Map.from(ops))

  inline def InfixLeft(
      expr: => Rule1[syntax.Expr],
      ops: Map[String, Operator]
  ): Rule1[syntax.Expr] = rule:
    expr ~ (WS ~ valueMap(ops) ~ WS ~ expr ~> ((_, _))).* ~> { (head, tail) =>
      tail.foldLeft(head) { (lhs, symRhs) =>
        val (sym, rhs) = symRhs
        given Span     = lhs.info <> rhs.info
        syntax.Expr.Bin(sym, lhs, rhs)
      }
    }

  // TODO: Compress these rules into an ordered table
  // See: https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Expr.html#v:makeExprParser

  def Expr1: Rule1[syntax.Expr] =
    InfixLeft(Expr2)("||" -> Operator.LogicalOr)

  def Expr2: Rule1[syntax.Expr] =
    InfixLeft(Expr3)("&&" -> Operator.LogicalAnd)

  def Expr3: Rule1[syntax.Expr] =
    InfixLeft(Expr4)(
      "==" -> Operator.Eql,
      "/=" -> Operator.NotEql,
      "<"  -> Operator.LessThan,
      "<=" -> Operator.LessThanOrEql,
      ">"  -> Operator.GreaterThan,
      ">=" -> Operator.GreaterThanOrEql
    )

  def Expr4: Rule1[syntax.Expr] =
    InfixLeft(Expr5)(
      "+" -> Operator.Add,
      "-" -> Operator.Sub
    )

  def Expr5: Rule1[syntax.Expr] =
    InfixLeft(Expr6)(
      "*" -> Operator.Mul,
      "/" -> Operator.Div
    )

  def Expr6: Rule1[syntax.Expr] =
    def typeArg: Rule1[syntax.Arg] = rule:
      push(cursor) ~ "[" ~ Type ~ "]" ~> { (start, tpe) =>
        syntax.Arg.Type(tpe)(using Span(start, cursor))
      }
    def valueArg: Rule1[syntax.Arg] = rule:
      AtomicExpr ~> (expr => syntax.Arg.Value(expr)(using expr.info))

    def Arg: Rule1[syntax.Arg] = rule(typeArg | valueArg)
    rule:
      AtomicExpr ~ (WS ~ Arg).* ~> { (fn, args) =>
        args.foldLeft(fn) { (app, arg) =>
          syntax.Expr.Apply(app, arg)(using app.info <> arg.info)
        }
      }

  def MatchArm: Rule1[syntax.MatchArm] = rule:
    push(cursor) ~ Pattern ~ ("if" ~ Expr1).? ~ "=>" ~ Expr ~> {
      (start, pattern, guard, body) =>
        syntax.MatchArm(pattern, body, guard)(using Span(start, cursor))
    }