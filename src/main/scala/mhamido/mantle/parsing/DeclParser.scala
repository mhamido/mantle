package mhamido.mantle.parsing

import mhamido.mantle.util.Span
import org.parboiled2.*
import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either

trait DeclParser extends BaseParser:
  self: ExprParser & PatternParser & TypeParser =>

  def Decl: Rule1[syntax.Decl] = rule:
    FunDecl | ValDecl | TypeDecl | DataDecl

  def Param: Rule1[syntax.Param] =
    def unitParam: Rule1[syntax.Param] = rule:
      push(cursor) ~ "(" ~ ")" ~ push(cursor) ~> { (start, end) =>
        given Span = Span(start, end)
        syntax.Param.Value("_", syntax.Type.Unit())
      }

    def typeParam: Rule1[syntax.Param] = rule:
      push(cursor) ~ "[" ~ PrimedName ~ "]" ~> { (start, name) =>
        syntax.Param.Type(name)(using Span(start, cursor))
      }

    def valParam: Rule1[syntax.Param] = rule:
      push(cursor) ~ "(" ~ LowerName ~ ":" ~ Type ~ ")" ~> {
        (start, name, tpe) =>
          syntax.Param.Value(name, tpe)(using Span(start, cursor))
      }

    rule(typeParam | unitParam | valParam)

  def FunDecl: Rule1[syntax.Decl] =
    def clause: Rule1[syntax.Decl.Fun] = rule:
      push(cursor) ~ LowerName ~ Param.+ ~ ":" ~ Type ~ "=" ~ Expr ~> {
        (start, name, params, ret, body) =>
          syntax.Decl.Fun(name, params, ret, body)(using Span(start, cursor))
      }
    rule:
      push(cursor) ~ "fun" ~!~ (clause + "and") ~> { (start, clauses) =>
        syntax.Decl.FunGroup(clauses)(using Span(start, cursor))
      }

  def ValDecl: Rule1[syntax.Decl] = rule:
    push(cursor) ~ "val" ~!~ Pattern ~ "=" ~ Expr ~> { (start, pattern, body) =>
      given Span = Span(start, cursor)
      syntax.Decl.Val(pattern, body)
    }

  def TypeParams: Rule1[Seq[String]] = rule:
    ("[" ~ (PrimedName + ",") ~ "]").? ~> { _.getOrElse(Seq.empty) }

  def TypeDecl: Rule1[syntax.Decl] = rule:
    push(cursor) ~ "type" ~!~ UpperName ~ TypeParams.? ~ "=" ~ Type ~> {
      (start, name, typeParams, tpe) =>
        syntax.Decl.Alias(name, typeParams.getOrElse(Seq.empty), tpe)(using
          Span(start, cursor)
        )
    }

  // TODO: Adopt OCaml style aliasing, otherwise
  // there wouldn't be a way to specify a mutually recursive alias + datatype
  def DataDecl: Rule1[syntax.Decl] =
    def constructor: Rule1[(String, Seq[syntax.Type])] = rule:
      UpperName ~ PrimType.? ~> { (name, tpe) =>
        (
          name,
          tpe.fold(Seq.empty) {
            case syntax.Type.Tuple(types) => types
            case xs                       => Seq(xs)
          }
        )
      }

    def data = rule:
      push(
        cursor
      ) ~ UpperName ~ TypeParams.? ~ ("=" ~ "|".? ~ (constructor + "|")).? ~> {
        (start, name, params, ctors) =>
          val typeParams   = params.getOrElse(Seq.empty)
          val constructors = ctors.getOrElse(Seq.empty)
          syntax.Decl.Data(name, typeParams, constructors)(using
            Span(start, cursor)
          )
      }

    rule:
      push(cursor) ~ "datatype" ~ (data + "and") ~> { (start, data) =>
        syntax.Decl.DataGroup(data)(using Span(start, cursor))
      }