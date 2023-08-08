package mhamido.mantle.parsing

import mhamido.mantle.util.Reporter
import scala.collection.BufferedIterator
import mhamido.mantle.util.Position

abstract class Parser(using val reporter: Reporter) {
  export mhamido.mantle.syntax.*

  def tokens: BufferedIterator[Token]
  def pos: Position = peek.pos

  def peek: Token      = tokens.head
  def advance(): Token = tokens.next()

  def isAtEnd: Boolean = !tokens.hasNext || (peek is Token.Eof)
  def matches(kinds: Token.Kind*): Boolean = kinds exists (peek is _)

  final inline def between[A](
      open: Token.Kind,
      sep: Token.Kind,
      close: Token.Kind
  )(elem: () => A): (Token, Seq[A], Token) = {
    def loop(): List[A] = expect {
      case kind if kind == close => Nil
      case kind if kind == sep =>
        val _ = advance()
        elem() :: loop()
    }

    val opened = consume(open).get
    val elems  = elem() :: loop()
    val closed = consume(close).get
    (opened, elems, closed)
  }

  def consume(kinds: Token.Kind*): Option[Token] =
    if matches(kinds*) then Some(advance())
    else
      reporter.error(
        Parser.Unexpected(kinds, tokens.headOption),
        peek.pos
      )
      None

  def peek[A](pf: Token ?=> Token.Kind => A): A =
    pf(using peek)(peek.kind)

  def expect[A](pf: Token ?=> PartialFunction[Token.Kind, A]): A =
    val token = peek
    // TODO: Handle parser recovery

    expect(fallback =
      reporter.fatalError(
        Parser.Unexpected(
          Token.Kind.values.toSeq.filter(pf(using token).isDefinedAt),
          Some(token)
        ),
        token.pos
      )
    )(pf)

  def expect[A](
      fallback: => A
  )(pf: Token ?=> PartialFunction[Token.Kind, A]): A =
    if pf(using peek) isDefinedAt peek.kind then
      val token = advance()
      pf(using token)(token.kind)
    else fallback
}

object Parser {
  sealed trait Error extends Exception

  case class Unexpected(
      expecting: Seq[Token.Kind],
      got: Option[Token]
  ) extends Error {
    override def getMessage(): String =
      val actual = got.map(tok =>
        s"Unexpected token ${tok.render} at line: ${tok.pos.line} col: ${tok.pos.column} "
      )
      val expected =
        s"Expected any of the following tokens: ${expecting map (r => s"'${r.render}'") mkString ","}."
      actual.fold(expected)(_ + expected)
  }
}
