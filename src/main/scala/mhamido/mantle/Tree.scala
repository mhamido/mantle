package mhamido.mantle

import mhamido.mantle.parsing.Token

abstract class Tree {
  type Name
  type Info
}

trait AbstractExpr extends Tree, AbstractValue {
  this: AbstractPattern & AbstractType & AbstractDecl =>

  sealed abstract class Expr(val info: Info)
  case class MatchArm(pattern: Pattern, body: Expr, guard: Option[Expr] = None)

  object Expr {
    export Value.*

    case class Not(
        expr: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class Negate(
        expr: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class Bin(
        op: Operator,
        left: Expr,
        right: Expr
    )(using override val info: Info)
        extends Expr(info)
    case class Ascribe(
        expr: Expr,
        tpe: Type
    )(using override val info: Info)
        extends Expr(info)

    case class Apply(
        fn: Expr,
        arg: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class Fn(
        params: Seq[Pattern],
        body: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class Let(
        defs: Seq[Decl],
        body: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class For(
        init: Pattern,
        start: Expr,
        finish: Expr,
        body: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class While(
        cond: Expr,
        body: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class If(
        cond: Expr,
        thenp: Expr,
        elsep: Expr
    )(using override val info: Info)
        extends Expr(info)

    case class Tuple(elems: Seq[Expr])(using override val info: Info)
        extends Expr(info)

    case class Case(
        scrutnee: Expr,
        cases: Seq[MatchArm]
    )(using override val info: Info)
        extends Expr(info)
  }
}

trait AbstractPattern extends Tree, AbstractValue {
  this: AbstractType & AbstractExpr & AbstractDecl =>
  sealed trait Pattern { def info: Info }

  object Pattern {
    export Value.*

    case class Wildcard(
    )(using override val info: Info)
        extends Pattern

    case class Tuple(
        elems: Seq[Pattern]
    )(using val info: Info)
        extends Pattern

    case class Alias(
        pat: Pattern,
        alias: Name
    )(using override val info: Info)
        extends Pattern

    case class Ascribe(
        pat: Pattern,
        tpe: Type
    )(using override val info: Info)
        extends Pattern

    case class Constructor(
        name: Name,
        args: Seq[Pattern]
    )(using override val info: Info)
        extends Pattern
  }
}

trait AbstractValue extends Tree {
  self: AbstractExpr & AbstractType & AbstractPattern & AbstractDecl =>

  sealed abstract class Value(override val info: Info)
      extends Expr(info),
        Pattern

  object Value {
    case class Unit()(using override val info: Info) extends Value(info)
    case class True()(using override val info: Info) extends Value(info)
    case class False()(using override val info: Info) extends Value(info)
    case class Var(name: Name)(using override val info: Info)
        extends Value(info)
    case class Int(value: scala.Int)(using override val info: Info)
        extends Value(info)
    case class Chr(value: scala.Char)(using override val info: Info)
        extends Value(info)
    case class String(value: Predef.String)(using override val info: Info)
        extends Value(info)
  }
}

trait AbstractDecl extends Tree {
  this: AbstractExpr & AbstractType & AbstractPattern =>

  sealed abstract class Decl(val info: Info)
  object Decl {
    case class TypeAlias(name: Name, typeParams: Seq[Name], tpe: Type)(using
        override val info: Info
    ) extends Decl(info)

    case class TypeDef(
        name: Name,
        params: Seq[Name],
        constructors: Seq[(Name, Seq[Name])]
    )(using override val info: Info)
        extends Decl(info)

    case class Val(
        pattern: Pattern,
        body: Expr
    )(using override val info: Info)
        extends Decl(info)

    case class Fun(
        name: Name,
        typeParams: Seq[Name],
        params: Seq[Pattern],
        ret: Type,
        body: Expr
    )(using override val info: Info)
        extends Decl(info)

    case class Mutual(decls: Seq[Decl])(using override val info: Info)
        extends Decl(info)
  }
}

trait AbstractType extends Tree {
  sealed abstract class Type { def info: Info }
  sealed abstract class Primitive(override val info: Info) extends Type
  object Type {
    case class Unit()(using override val info: Info) extends Primitive(info)
    case class Bool()(using override val info: Info) extends Primitive(info)
    case class String()(using override val info: Info) extends Primitive(info)
    case class Integer()(using override val info: Info) extends Primitive(info)
    case class Var(in: Name)(using override val info: Info) extends Type
    case class Array(elem: Type)(using override val info: Info)
        extends Primitive(info)
    case class Fn(in: Type, out: Type)(using override val info: Info)
        extends Type
    case class Named(tycon: Name)(using override val info: Info) extends Type
    case class App(tyCon: Type, args: Seq[Type])(using override val info: Info)
        extends Type
    case class Tuple(tpes: Seq[Type])(using override val info: Info)
        extends Type
  }
}

trait AbstractModule
    extends Tree,
      AbstractDecl,
      AbstractExpr,
      AbstractType,
      AbstractPattern {
  case class Module(name: Seq[Name], decls: Seq[Decl])
}

enum Operator {
  case Eql, NotEql
  case Not, LogicalAnd, LogicalOr
  case Add, Sub, Mul, Div
  case LessThan, GreaterThan, LessThanOrEql, GreaterThanOrEql
  case Dot, Semi, Colon, Comma
  case ThinArrow, ThickArrow, LeftArrow
}

object Operator {
  // There has to be a better way to do this, but I'm content with this for now.
  inline def apply(kind: Token.Kind): Operator =
    Operator.values.find(_.toString == kind.toString).get

  lazy val tokenKinds: Seq[Token.Kind] =
    Operator.values.iterator.flatMap { op =>
      try Some(Token.Kind.valueOf(op.productPrefix))
      catch {
        case e: IllegalArgumentException => None
      }
    }.toList
}
