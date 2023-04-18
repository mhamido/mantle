package mhamido.mantle.util
import mainargs.TokensReader
import mainargs.{main, arg, ParserForClass, Flag}

sealed abstract class Target

object Target:
  case object Compiler extends Target
  case object Interpreter extends Target

@mainargs.main
final case class Context(
    @arg(short = 'i', name = "input", doc = "Source files to be compiled.")
    val inputFiles: Seq[os.Path],
    @arg(
      short = 'o',
      name = "output",
      doc = "Path for the output/object to be placed in."
    )
    val outFiles: Seq[os.Path] = List(os.pwd),
    @arg(
      name = "ddump-tokens",
      doc = "Print out tokens produced by the lexer."
    )
    val dumpTokens: Boolean,
    @arg(
      name = "ddump-ast",
      doc = "Print out syntax trees produced by the parser."
    )
    val dumpSyntaxTree: Boolean,
    @arg(
      short = 'O',
      name = "optimize",
      doc = "Apply optimizations to the intermediate representation."
    )
    val optimize: Boolean
)(using val reporter: Reporter)