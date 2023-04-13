package mhamido.mantle.syntax

import mhamido.mantle.util.Span

trait Node {
  type Info
  type Name
  type Binder = Name
}

trait AbstractExpr extends Node, AbstractValue {
  this: AbstractPattern & AbstractType =>

  sealed abstract class Expr(info: Info)
  case class MatchArm(pattern: Pattern, body: Expr, guard: Option[Expr] = None)

  object Expr {
    export Value.*

    case class Not(
        expr: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Negate(
        expr: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Bin(
        op: Operator,
        left: Expr,
        right: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Con(
        name: Name
    )(using val info: Info)
        extends Expr(info)

    case class Ascribe(
        expr: Expr,
        tpe: Type
    )(using val info: Info)
        extends Expr(info)

    case class Apply(
        fn: Expr,
        arg: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Fn(
        params: Seq[Pattern],
        body: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Let(
        defs: Seq[Decl],
        body: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Abs(
        expr: Expr,
        tpe: Type,
        body: Expr
    )(using val info: Info)
        extends Expr(info)

    case class While(
        cond: Expr,
        body: Expr
    )(using val info: Info)
        extends Expr(info)

    case class If(
        cond: Expr,
        thenp: Expr,
        elsep: Expr
    )(using val info: Info)
        extends Expr(info)

    case class Case(
        scrutnee: Expr,
        cases: Seq[MatchArm]
    )(using val info: Info)
        extends Expr(info)
  }
}

trait AbstractPattern extends Node, AbstractValue {
  this: AbstractType =>
  sealed trait Pattern(info: Info)

  object Pattern {
    export Value.*

    case class Wildcard(
    )(using val info: Info)
        extends Pattern(info)

    case class Tuple(
        elems: Seq[Pattern]
    )(using val info: Info)
        extends Pattern(info)

    case class Alias(
        pat: Pattern,
        alias: Name
    )(using val info: Info)
        extends Pattern(info)

    case class Ascribe(
        pat: Pattern,
        tpe: Type
    )(using val info: Info)
        extends Pattern(info)

    case class Constructor(
        name: Name,
        args: Seq[Pattern]
    )(using val info: Info)
        extends Pattern(info)
  }
}

trait AbstractValue extends Node {
  this: AbstractExpr & AbstractPattern =>
  sealed abstract class Value(val info: Info) extends Expr(info), Pattern(info)

  object Value {
    case class Unit()(using val info: Info) extends Value(info)
    case class True()(using val info: Info) extends Value(info)
    case class False()(using val info: Info) extends Value(info)
    case class Var(name: Name)(using val info: Info) extends Value(info)
    case class Int(value: scala.Int)(using val info: Info) extends Value(info)
    case class Chr(value: scala.Char)(using val info: Info) extends Value(info)
    case class String(value: Predef.String)(using val info: Info)
        extends Value(info)
  }
}

trait AbstractDecl extends Node {
  this: AbstractExpr & AbstractType =>

  sealed abstract class Param(name: Name)
  case class TypeParam(name: Name) extends Param(name)
  case class ValueParam(name: Name, tpe: Type) extends Param(name)

  sealed abstract class Decl(val info: Info)
  object Decl {
    case class Type(name: Binder, typeParams: Seq[Name], tpe: Type)
    case class Fun(name: Name, params: Seq[Param])
    case class Mutual(decls: Seq[Decl])
  }
}

trait AbstractType extends Node {
  sealed trait Type
  sealed trait Primitive extends Type

  object Type {
    case object Unit extends Primitive
    case object Char extends Primitive
    case object Int extends Primitive
    case object String extends Primitive
    case object Array extends Primitive
    case class Named(name: Name) extends Type
    case class Var(name: Name.TypeVar) extends Type
    case class Arrow(from: Type, to: Type) extends Type
    case class Tuple(tpes: Seq[Type]) extends Primitive
    case class Apply(con: Type, args: Seq[Type]) extends Type
    case class Record(fields: Map[Name, Type]) extends Type
  }
}

object syntax extends AbstractExpr, AbstractPattern {
  type Name = String
  type Info = Span
}
