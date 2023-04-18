package mhamido.mantle

import mhamido.mantle.util.Span
import mhamido.mantle.syntax.Operator

abstract class Tree {
  type Info
  type Name
//   type Binder = Name
}

trait AbstractExpr extends Tree, AbstractValue {
  this: AbstractPattern & AbstractType & AbstractDecl =>

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

trait AbstractPattern extends Tree, AbstractValue {
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

trait AbstractValue extends Tree {
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

trait AbstractDecl extends Tree {
  this: AbstractExpr & AbstractType =>

  sealed abstract class Param(name: Name)
  case class TypeParam(name: Name) extends Param(name)
  case class ValueParam(name: Name, tpe: Type) extends Param(name)

  sealed abstract class Decl(val info: Info)
  object Decl {
    case class Type(name: Name, typeParams: Seq[Name], tpe: Type)
    case class Fun(name: Name, params: Seq[Param])
    case class Mutual(decls: Seq[Decl])
  }
}

trait AbstractType extends Tree {
  sealed abstract class Type(info: Info)
  sealed abstract class Primitive(info: Info) extends Type(info)
  object Type {
    case class Unit()(using info: Info) extends Primitive(info)
    case class Bool()(using info: Info) extends Primitive(info)
    case class String()(using info: Info) extends Primitive(info)
    case class Integer()(using info: Info) extends Primitive(info)
    case class Var(in: Name)(using info: Info) extends Type(info)
    case class Array(elem: Type)(using info: Info) extends Primitive(info)
    case class Fn(in: Type, out: Type)(using info: Info) extends Type(info)
    case class Named(tycon: Name)(using info: Info) extends Type(info)
    case class TypeFn(in: Name, out: Type)(using info: Info)
        extends Type(info)
    case class App(tyCon: Type, args: Seq[Type])(using info: Info)
        extends Type(info)
  }
}

trait AbstractModule
    extends Tree,
      AbstractDecl,
      AbstractExpr,
      AbstractType,
      AbstractPattern {
  case class Module(names: Seq[Name], decls: Seq[Decl])
}
