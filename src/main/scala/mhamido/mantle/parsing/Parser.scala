package mhamido.mantle.parsing

import mhamido.mantle.util.Reporter
import scala.collection.BufferedIterator
import mhamido.mantle.util.Position

abstract class Parser(using val reporter: Reporter) {
  export mhamido.mantle.syntax.*

  def tokens: BufferedIterator[Token]

  def pos: Position = peek.pos

  def peek: Token = tokens.head
  def advance(): Token = tokens.next()

  def isAtEnd: Boolean = !tokens.hasNext || (peek is Token.Eof)
  def matches(kinds: Token.Kind*): Boolean = kinds exists (peek is _)

  def consume(kind: Token.Kind): Option[Token] =
    Option.when(matches(kind))(advance())

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
      val actual = got.map(tok => s"Unexpected token: ${tok.render} ")
      val expected =
        s"Expected any of the following tokens: ${expecting map (r => s"'${r.render}'") mkString ","}."
      actual.fold(expected)(_ + expected)
  }
}
