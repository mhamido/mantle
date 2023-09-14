package mhamido.mantle.parsing

import mhamido.mantle.util.Span
import org.parboiled2.*
import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either

trait TypeParser extends BaseParser:
  def Type: Rule1[syntax.Type] =
    def typeFn: Rule1[syntax.Type] = rule:
      push(cursor) ~ "[" ~ PrimedName ~ "]" ~ "->" ~ Type ~> {
        (start, param, body) =>
          given Span = Span(start, cursor)
          syntax.Type.TypeFn(param, body)
      }

    def fn: Rule1[syntax.Type] = rule:
      push(cursor) ~ ParameterizedType ~ ("->" ~ Type).? ~> {
        (start, param, result) =>
          result.fold(param)(
            syntax.Type.Fn(param, _)(using Span(start, cursor))
          )
      }

    rule(typeFn | fn)

  def ParameterizedType: Rule1[syntax.Type] = rule:
    push(cursor) ~ PrimType ~ ("[" ~ (Type + ",") ~ "]").? ~> {
      (start, tpe, paramList) =>
        paramList.fold(tpe)(
          syntax.Type.Apply(tpe, _)(using Span(start, cursor))
        )
    }

  def PrimType: Rule1[syntax.Type] =
    def tupledType: Rule1[syntax.Type] = rule:
      push(cursor) ~ "(" ~ (Type + ",") ~ ")" ~> { (start, types) =>
        assert(types.nonEmpty)
        if types.lengthIs == 1 then types.head
        else syntax.Type.Tuple(types)(using Span(start, cursor))
      }

    def typeVar: Rule1[syntax.Type] = rule:
      push(cursor) ~ PrimedName ~> { (start, name) =>
        syntax.Type.Var(name)(using Span(start, cursor))
      }

    def namedType: Rule1[syntax.Type] = rule:
      push(cursor) ~ (UpperName | LowerName) ~> { (start, name) =>
        given Span = Span(start, cursor)
        name match
          case "Int"    => syntax.Type.Int()
          case "Unit"   => syntax.Type.Unit()
          case "Array"  => syntax.Type.Array()
          case "Bool"   => syntax.Type.Bool()
          case "String" => syntax.Type.String()
          case name     => syntax.Type.Named(name)
      }

    rule(tupledType | typeVar | namedType)