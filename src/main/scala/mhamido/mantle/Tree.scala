package mhamido.mantle

import mhamido.mantle.parsing.Token
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig

trait AbstractModule { self =>
  type Name
  type Info

  sealed abstract class Tagged { def info: Info }

  sealed abstract class Type extends Tagged
  object Type {
    given codec[Info: JsonValueCodec, Name: JsonValueCodec]
        : JsonValueCodec[Type] =
      JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

    case class Var(name: Name)(using val info: Info)                extends Type
    case class Fn(param: Type, ret: Type)(using val info: Info)     extends Type
    case class TypeFn(param: Name, ret: Type)(using val info: Info) extends Type
    case class Named(name: Name)(using val info: Info)              extends Type
    case class Apply(constr: Type, args: Seq[Type])(using
        val info: Info
    ) extends Type
    case class Int()(using val info: Info)                   extends Type
    case class Unit()(using val info: Info)                  extends Type
    case class Bool()(using val info: Info)                  extends Type
    case class String()(using val info: Info)                extends Type
    case class Array()(using val info: Info)                 extends Type
    case class Tuple(types: Seq[Type])(using val info: Info) extends Type
  }

  // TODO These exist just to work around the fact that union types aren't serializable yet.
  sealed abstract class Param extends Tagged
  object Param {
    given JsonValueCodec[Param] = JsonCodecMaker.make
    case class Type(name: Name)(using val info: Info) extends Param
    case class Value(name: Name, tpe: self.Type)(using val info: Info)
        extends Param
  }

  sealed abstract class Arg extends Tagged
  object Arg {
    given JsonValueCodec[Arg] = JsonCodecMaker.make
    case class Type(name: Name)(using val info: Info)   extends Arg
    case class Value(value: Expr)(using val info: Info) extends Arg
  }

  sealed trait Pattern      extends Tagged
  sealed trait BasicPattern extends Tagged
  object Pattern {
    export Value.*
    given codec: JsonValueCodec[Decl] = JsonCodecMaker.make
    case class Wildcard()(using val info: Info) extends BasicPattern
    case class Constr(name: Name, typeArgs: Seq[Type], args: Seq[BasicPattern])(
        using val info: Info
    ) extends Pattern
  }

  sealed abstract class Decl extends Tagged
  object Decl {
    given codec: JsonValueCodec[Decl] = JsonCodecMaker.make

    case class Val(binder: BasicPattern, tpe: Option[Type], body: Expr)(using
        override val info: Info
    ) extends Decl
    case class Alias(name: Name, typeParams: Seq[Name], tpe: Type)(using
        override val info: Info
    ) extends Decl
    case class FunGroup(defs: Seq[Fun])(using val info: Info)   extends Decl
    case class DataGroup(defs: Seq[Data])(using val info: Info) extends Decl

    case class Fun(name: Name, params: Seq[Param], ret: Type, body: Expr)(using
        val info: Info
    ) extends Tagged
    case class Data(
        name: Name,
        typeParams: Seq[Name],
        constructors: Seq[(Name, Seq[Type])]
    )(using val info: Info)
        extends Tagged
  }

  sealed abstract class Expr  extends Tagged
  sealed abstract class Value extends Expr, Pattern
  object Value {
    case class Unit()(using val info: Info)          extends Value
    case class True()(using val info: Info)          extends Value
    case class False()(using val info: Info)         extends Value
    case class Var(name: Name)(using val info: Info) extends Value, BasicPattern
    case class Int(value: scala.Int)(using val info: Info)        extends Value
    case class Chr(value: Char)(using val info: Info)             extends Value
    case class String(value: Predef.String)(using val info: Info) extends Value
  }

  case class MatchArm(pattern: Pattern, body: Expr, guard: Option[Expr] = None)

  object Expr {
    export Value.*
    given [Info: JsonValueCodec, Name: JsonValueCodec]: JsonValueCodec[Expr] =
      JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

    case class Not(
        expr: Expr
    )(using val info: Info)
        extends Expr

    case class Negate(
        expr: Expr
    )(using val info: Info)
        extends Expr

    case class Bin(
        op: Operator,
        left: Expr,
        right: Expr
    )(using val info: Info)
        extends Expr

    case class Ascribe(
        expr: Expr,
        tpe: Type
    )(using val info: Info)
        extends Expr

    case class Apply(
        fn: Expr,
        arg: Arg
    )(using val info: Info)
        extends Expr

    case class Fn(
        params: Seq[Param],
        body: Expr
    )(using val info: Info)
        extends Expr

    case class Let(
        defs: Seq[Decl],
        body: Expr
    )(using val info: Info)
        extends Expr

    case class If(
        cond: Expr,
        thenp: Expr,
        elsep: Expr
    )(using val info: Info)
        extends Expr

    case class Constr(
        name: Name,
        typeArgs: Seq[Type],
        args: Seq[Expr]
    )(using val info: Info)
        extends Expr

    case class Case(
        scrutnee: Expr,
        cases: Seq[MatchArm]
    )(using val info: Info)
        extends Expr

    case class Tuple(
        elems: Seq[Expr]
    )(using val info: Info)
        extends Expr
  }

  case class Module(name: Seq[Name], decls: Seq[Decl])
  object Module {
    given [Info: JsonValueCodec, Name: JsonValueCodec]: JsonValueCodec[Module] =
      JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
  }
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
