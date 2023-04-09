package mhamido.mantle.syntax

sealed trait Type

object Type:
  sealed trait Primitive extends Type

  case object Unit extends Primitive
  case object Char extends Primitive
  case object Int extends Primitive
  case object String extends Primitive
  case object Array extends Primitive

  // TODO: floating point
  // case object Float32 extends Primitive

  case class Named(name: Name) extends Type
  case class TypeVar(name: Name.TypeVar) extends Type
  case class Arrow(from: Type, to: Type) extends Type
  case class Tuple(tpes: Seq[Type]) extends Primitive
  case class Apply(con: Type, args: Type) extends Type
  case class Record(fields: Map[Name, Type]) extends Type
  // TODO: Replace these with overridable definitions inside
  // the standard lib
  val Primitives: Map[Name, Primitive] = Map(
    Name.Constructor("Int") -> Int,
    Name.Constructor("Char") -> Char,
    Name.Constructor("String") -> String,
    Name.Constructor("Array") -> Array
  )
