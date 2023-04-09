package mhamido.mantle.syntax

sealed trait Name:
  def name: String

  def show: String = this match
    case Name.TypeVar(name) => s"'$name"
    case _                  => name

object Name:
  case object Error extends Name {
    def name: String = ???
  }

  case class Var(name: String) extends Name
  case class TypeVar(name: String) extends Name
  case class Constructor(name: String) extends Name

  case class Qualified(prefixes: Seq[String], entity: Name) extends Name:
    def name: String = prefixes.mkString("", ".", entity.name)

  