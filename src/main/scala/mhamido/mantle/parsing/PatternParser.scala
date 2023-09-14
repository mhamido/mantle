package mhamido.mantle.parsing

import mhamido.mantle.util.Span
import org.parboiled2.*
import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either

trait PatternParser extends BaseParser:
  self: TypeParser =>

  def Pattern: Rule1[syntax.Pattern] = rule:
    AscribedPattern

  def TupledPattern: Rule1[syntax.Pattern] = rule:
    push(cursor) ~ "(" ~ (Pattern * ",") ~ ")" ~> { (start, elems) =>
      given Span = Span(start, cursor)
      if elems.isEmpty then syntax.Pattern.Unit()
      else if elems.lengthIs == 1 then elems.head
      else syntax.Pattern.Tuple(elems)
    }

  def BasicPattern: Rule1[syntax.Pattern] =
    def varname = rule:
      push(cursor) ~ LowerName ~> { (start, name) =>
        given Span = Span(start, cursor)
        name match
          case "true"  => syntax.Pattern.True()
          case "false" => syntax.Pattern.False()
          case _       => syntax.Pattern.Var(name)
      }
    def constructor = rule:
      push(
        cursor
      ) ~ UpperName ~ ("[" ~ (Type + ",") ~ "]").? ~ Pattern.? ~> {
        (start, name, typeArgs, args) =>
          given Span = Span(start, cursor)
          val tyArgs = typeArgs.getOrElse(Seq.empty)
          syntax.Pattern.Constr(name, tyArgs, args)
      }

    def wildcard = rule:
      push(cursor) ~ "_" ~> { start =>
        syntax.Pattern.Wildcard()(using Span(start, cursor))
      }

    def int = rule:
      push(cursor) ~ Integer ~> { (start, value) =>
        syntax.Pattern.Int(value)(using Span(start, cursor))
      }

    def char = rule:
      push(cursor) ~ CharLiteral ~> { (start, value) =>
        syntax.Pattern.Chr(value.head)(using Span(start, cursor))
      }

    def string = rule:
      push(cursor) ~ StringLiteral ~> { (start, value) =>
        syntax.Pattern.String(value)(using Span(start, cursor))
      }

    rule(wildcard | int | char | string | varname | constructor | TupledPattern)

  def AscribedPattern: Rule1[syntax.Pattern] = rule:
    push(cursor) ~ BasicPattern ~ (":" ~ Type).? ~> {
      (start, pattern, ascription) =>
        given Span = Span(start, cursor)
        ascription.fold(pattern)(syntax.Pattern.Ascribe(pattern, _))
    }