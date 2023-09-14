package mhamido.mantle
package parsing

import mhamido.mantle.util.Span
import org.parboiled2.*

import scala.language.implicitConversions
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context
import os.Path
import Parser.DeliveryScheme.Either

// TODO: add .named annotations for nicer error messages

class ModuleParser(val input: ParserInput)
    extends BaseParser,
      PatternParser,
      TypeParser,
      DeclParser,
      ExprParser:

  def Module: Rule1[syntax.Module] = rule:
    "module" ~ QualifiedName ~ "with" ~ Decl.* ~ EOI ~> {
      syntax.Module(_, _)
    }

  def QualifiedName: Rule1[Seq[String]] = rule:
    UpperName ~ zeroOrMore("." ~ UpperName) ~> { _ +: _ }

  def ReplTerm: Rule1[syntax.Decl | syntax.Expr] = rule:
    Decl | Expr

object ModuleParser extends Phase[os.Path, syntax.Module]:
  override def apply(in: Path)(using ctx: Context): syntax.Module =
    val contents = os.read(in, "utf-8")
    val parser   = new ModuleParser(contents)
    val result   = parser.Module.run()
    result match
      case Left(parseErr) =>
        val err = parser.formatError(parseErr)
        ctx.reporter.fatalError(err)
      case Right(value) =>
        value

  def apply(line: String)(using
      ctx: Context
  ): Either[String, syntax.Decl | syntax.Expr] =
    val parser = new ModuleParser(line)
    val result = parser.ReplTerm.run()
    result.left.map(parser.formatError(_))
