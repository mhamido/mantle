package mhamido.mantle

import mhamido.mantle.parsing.Token

trait AbstractModule { self =>
  type Name
  type Info
  abstract class Node { def info: Info }

  def primNameFactory[A <: Node](prim: String): Name

  sealed abstract class Type extends Node
  object Type {
    case class Var(name: Name)(using override val info: Info) extends Type
    case class Fn(param: Type, ret: Type)(using override val info: Info)
        extends Type
    case class TypeFn(param: Name, ret: Type)(using override val info: Info)
        extends Type
    case class Constr(tpe: Name, args: Seq[Type])(using
        override val info: Info
    ) extends Type

    final val IntName: Name    = primNameFactory[Type]("Int")
    final val UnitName: Name   = primNameFactory[Type]("Unit")
    final val BoolName: Name   = primNameFactory[Type]("Bool")
    final val ArrayName: Name  = primNameFactory[Type]("Array")
    final val StringName: Name = primNameFactory[Type]("String")

    def Int(using info: Info): Type              = Constr(IntName, Seq.empty)
    def Unit(using info: Info): Type             = Constr(UnitName, Seq.empty)
    def Bool(using info: Info): Type             = Constr(BoolName, Seq.empty)
    def String(using info: Info): Type           = Constr(StringName, Seq.empty)
    def Array(tpe: Type)(using info: Info): Type = Constr(ArrayName, Seq(tpe))
  }

  sealed abstract class Param extends Node

  object Param {
    case class Type(name: Name)(using override val info: Info) 
        extends Param
    case class Value(name: Name, tpe: self.Type)(using override val info: Info)
        extends Param
  }

  sealed trait Pattern      extends Node
  sealed trait BasicPattern extends Node
  object Pattern {
    export Value.*
    case class Wildcard()(using override val info: Info) extends BasicPattern
    case class Constr(name: Name, typeArgs: Seq[Type], args: Seq[BasicPattern])(
        using override val info: Info
    ) extends Pattern
  }

  sealed abstract class Decl extends Node
  object Decl {
    case class Val(binder: BasicPattern, tpe: Type, body: Expr)(using
        override val info: Info
    ) extends Decl
    case class Alias(name: Name, typeParams: Seq[Name], tpe: Type)(using
        override val info: Info
    ) extends Decl
    case class FunGroup(defs: Seq[Fun])(using override val info: Info)
        extends Decl
    case class DataGroup(defs: Seq[Data])(using override val info: Info)
        extends Decl

    case class Fun(name: Name, params: Seq[Param], ret: Type, body: Expr)(using
        override val info: Info
    )
    case class Data(
        name: Name,
        typeParams: Seq[Name],
        constructors: Seq[(Name, Seq[Type])]
    )(using override val info: Info)
  }

  sealed abstract class Expr  extends Node
  sealed abstract class Value extends Expr, Pattern
  object Value {
    case class Var(name: Name)(using override val info: Info)
        extends Value,
          BasicPattern
    case class Int(value: scala.Int)(using override val info: Info)
        extends Value
    case class String(value: Predef.String)(using override val info: Info)
        extends Value
    case class True()(using override val info: Info)  extends Value
    case class False()(using override val info: Info) extends Value
  }

  case class MatchArm(pattern: Pattern, body: Expr, guard: Option[Expr] = None)

  object Expr {
    export Value.*

    case class Do(seq: Seq[Expr])(using override val info: Info) extends Expr

    case class Not(
        expr: Expr
    )(using override val info: Info)
        extends Expr

    case class Negate(
        expr: Expr
    )(using override val info: Info)
        extends Expr

    case class Bin(
        op: Operator,
        left: Expr,
        right: Expr
    )(using override val info: Info)
        extends Expr

    case class Ascribe(
        expr: Expr,
        tpe: Type
    )(using override val info: Info)
        extends Expr

    case class Apply(
        fn: Expr,
        arg: Expr | Type
    )(using override val info: Info)
        extends Expr

    case class Fn(
        params: Seq[Param],
        body: Expr
    )(using override val info: Info)
        extends Expr

    case class Let(
        defs: Seq[Decl],
        body: Expr
    )(using override val info: Info)
        extends Expr

    case class If(
        cond: Expr,
        thenp: Expr,
        elsep: Expr
    )(using override val info: Info)
        extends Expr

    case class Constr(
        name: Name,
        typeArgs: Seq[Type],
        args: Seq[Expr]
    )(using override val info: Info)
        extends Expr

    case class Case(
        scrutnee: Expr,
        cases: Seq[MatchArm]
    )(using override val info: Info)
        extends Expr
  }

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
