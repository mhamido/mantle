package mhamido.mantle.syntax

import mhamido.mantle.syntax.Operator

sealed trait Expr
sealed trait Pattern
sealed trait Value extends Expr, Pattern

object Expr:
  case class MatchArm(pattern: Pattern, body: Expr, guard: Option[Expr] = None) 

  case object Unit extends Value

  // TODO: Implement these in the standard lib
  case object True extends Value
  case object False extends Value
  // and these as function applications
  case class Not(expr: Expr) extends Expr
  case class Negate(expr: Expr) extends Expr
  case class Bin(op: Operator, left: Expr, right: Expr) extends Expr

  case class Var(name: Name) extends Value
  case class Int(value: scala.Int) extends Value
  case class Chr(value: scala.Char) extends Value
  case class String(value: Predef.String) extends Value

  case class Con(name: Name) extends Expr
  case class Ann(expr: Expr, tpe: Type) extends Expr
  case class Apply(fn: Expr, arg: Expr) extends Expr
    
  case class Fn(params: Seq[Pattern], body: Expr) extends Expr
  case class Let(defs: Seq[Decl], body: Expr) extends Expr
  case class Abs(expr: Expr, tpe: Type, body: Expr) extends Expr
  case class While(cond: Expr, body: Expr) extends Expr
  case class If(cond: Expr, thenp: Expr, elsep: Expr) extends Expr
  case class Case(scrutnee: Expr, cases: Seq[MatchArm]) extends Expr
object Pattern:
  export Expr.{Unit, True, False, Int, String, Var}

  case object Wildcard extends Pattern
  case class Tuple(elems: Seq[Pattern]) extends Pattern
  case class Alias(pat: Pattern, alias: Name) extends Pattern
  case class Ascribe(pat: Pattern, tpe: Type) extends Pattern
  case class Constructor(name: Name, args: Seq[Pattern]) extends Pattern
