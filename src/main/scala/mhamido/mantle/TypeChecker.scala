package mhamido.mantle

object typed extends AbstractModule {
  type Name = syntax.Name
  type Info = Type
}

class TypeChecker() {
  import syntax.{Expr, Decl, Type}

  type Env = Map[syntax.Name, typed.Type]
  case class Typecheck[A]()

  def infer(expr: syntax.Expr)(using env: Env): Typecheck[typed.Expr] = expr match
    case syntax.Expr.Not(value) => {
      val tpedValue = check(value, syntax.Type.Boolean)
      pure { }
    }

  

  def check(expr: syntax.Expr, expected: typed.Type)(using
      env: Env
  ): Typecheck[typed.Expr] = ???

  def check(decl: syntax.Decl)(using env: Env): Typecheck[typed.Decl] = ???

  def check(pattern: syntax.Pattern)(using env: Env): Typecheck[typed.Pattern] =
    ???
}

object TypeChecker
