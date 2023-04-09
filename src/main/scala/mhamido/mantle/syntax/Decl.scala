package mhamido.mantle.syntax

import scala.deriving.Mirror

sealed trait Decl

object Decl:
  final case class Constructor(name: Name.Constructor, args: Seq[Type])

  case class Function(
      name: Name.Var,
      typeParams: Seq[Name],
      params: Seq[Pattern],
      ret: Option[Type],
      body: Expr
  ) extends Decl

  case class TypeDef(
      name: Name.Constructor,
      params: Seq[Name.TypeVar],
      constructors: Seq[Constructor]
      //   derivingInstances: Seq[Name.Constructor]
  ) extends Decl

  case class TypeAlias(
      name: Name.Constructor,
      params: Seq[Name.TypeVar],
      body: Type
  ) extends Decl

  case class Val(
      binder: Pattern,
      body: Expr
  ) extends Decl

  case class Mutual(
      defs: Seq[Decl]
  ) extends Decl

  // TODO: interface, instance
