package mhamido.mantle

import scala.collection.immutable.HashMap
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context

object typed extends AbstractModule {
  type Name = Symbol
  type Info = Type
}

class TypeChecker:
  import TypeChecker.*
  import syntax.{Decl, Expr, Type, Param}

  given Conversion[syntax.Type, typed.Type] = ???
  given Conversion[syntax.Name, typed.Name] = Symbol(_)

  def infer(expr: Expr)(using Environment): typed.Expr =
    given typed.Info = ???
    expr match
      case Expr.Unit()                => typed.Expr.Unit()
      case Expr.True()                => typed.Expr.True()
      case Expr.False()               => typed.Expr.False()
      case Expr.Var(name)             => ???
      case Expr.Int(value)            => typed.Expr.Int(value)
      case Expr.Chr(value)            => typed.Expr.Chr(value)
      case Expr.String(value)         => typed.Expr.String(value)
      case Expr.Not(subexpr)          => ???
      case Expr.Negate(subexpr)       => ???
      case Expr.Bin(op, lhs, rhs)     => ???
      case Expr.Ascribe(subexpr, tpe) => check(subexpr, tpe)
      case Expr.Apply(fn, arg)        => ???
      case Expr.If(cond, thenp, elsep) =>
        val typedCond = check(cond, typed.Type.Bool())
        val typedThen = infer(thenp)
        val typedElse = check(elsep, typedThen.info)
        typed.Expr.If(typedCond, typedThen, typedElse)(using typedThen.info)
      case Expr.Match(scrutinee, cases) => ???
      case Expr.Tuple(info)             => ???

  def check(decl: Decl)(using Environment): typed.Decl = decl match
    case Decl.Val(binder, body) =>
      val typedBody   = infer(body)
      val typedBinder = check(binder, typedBody.info)
      typed.Decl.Val(typedBinder, typedBody)(using typedBody.info)

    case Decl.Alias(name, typeParams, tpe) => ???

    case Decl.FunGroup(fns) =>
      val signatures = fns.map { case Decl.Fun(name, params, ret, _) =>
        val tpe = params.foldRight(ret: typed.Type) {
          case (Param.Type(name), ty) => typed.Type.TypeFn(name, ty)(using ???)
          case (Param.Value(_, paramTy), ty) =>
            typed.Type.Fn(paramTy, ty)(using ???)
        }
        name -> tpe
      }
      // todo: extend the env with the signatures of the fns
      // then check the body of each fn
      ???

    case Decl.DataGroup(fns) => ???

  def check(expr: syntax.Expr, tpe: typed.Type)(using
      Environment
  ): typed.Expr = ???

  def check(pat: syntax.Pattern, tpe: typed.Type)(using
      Environment
  ): typed.Pattern = ???

object TypeChecker extends Phase[syntax.Module, typed.Module]:
  type Environment = Map[Symbol, typed.Type]
  override def apply(in: syntax.Module)(using Context): typed.Module = ???
