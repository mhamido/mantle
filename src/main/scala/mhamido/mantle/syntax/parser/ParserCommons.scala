package mhamido.mantle.syntax.parser

import mhamido.mantle.Token
import mhamido.mantle.util.Reporter
import mhamido.mantle.util.Position

abstract class ParserCommons(using val reporter: Reporter) {
  import ParserCommons.Unexpected

  protected def path: os.Path
  protected def tokens: BufferedIterator[Token]

  def peek: Token = tokens.head
  def advance(): Token = tokens.next()

  def isAtEnd: Boolean = peek.kind == Token.Eof
  def matches(kinds: Token.Kind*): Boolean = kinds.contains(peek.kind)

  def consume(kind: Token.Kind*): Option[Token] =
    val tok = peek

    if matches(kind: _*) then Some(tokens.next())
    else
      reporter.error(Unexpected(kind, Some(tok)), tok.pos)
      None

  def peek[A](pf: Token ?=> Token.Kind => A): A =
    pf(using peek)(peek.kind)

  def expect[A](pf: Token ?=> PartialFunction[Token.Kind, A]): A =
    val token = peek
    // TODO: Handle parser recovery

    expect(fallback =
      reporter.fatalError(
        Unexpected(
          Token.Kind.values.filter(pf(using token).isDefinedAt),
          Some(token)
        ),
        token.pos
      )
    )(pf)

  def expect[A](
      fallback: => A
  )(pf: Token ?=> PartialFunction[Token.Kind, A]): A =
    if pf(using peek) isDefinedAt peek.kind then
      val token = tokens.next()
      pf(using token)(token.kind)
    else fallback
}

object ParserCommons {
  case class Unexpected(
      expecting: Seq[Token.Kind],
      got: Option[Token]
  ) extends Exception() {
    override def getMessage(): String =
      val actual = got.map(tok => s"Unexpected token: ${tok.render} ")
      val expected =
        s"Expected any of the following tokens: ${expecting map (r => s"'${r.render}'") mkString ","}."
      actual.fold(expected)(_ + expected)
  }
}
