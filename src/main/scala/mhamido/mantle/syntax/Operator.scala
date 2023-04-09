package mhamido.mantle.syntax

import scala.quoted.{Quotes}
import mhamido.mantle.Token
import scala.quoted.Expr
import scala.quoted.FromExpr

// There has to be a better way to do this, but I'm content with this until nested enums work.

enum Operator {
  case Eql, NotEql
  case Not, LogicalAnd, LogicalOr
  case Add, Sub, Mul, Div
  case LessThan, GreaterThan, LessThanOrEql, GreaterThanOrEql
  case Dot, Semi, Colon, Comma
  case ThinArrow, ThickArrow, LeftArrow
}

object Operator {
  inline def apply(kind: Token.Kind): Operator =
    Operator.values.find(_.toString == kind.toString).get
}
