package mhamido.mantle.parsing

import mhamido.mantle.util.Context
import mhamido.mantle.util.Phase
import mhamido.mantle.util.Position
import mhamido.mantle.util.Reporter
import os.Path

import scala.annotation.switch
import scala.collection.BufferedIterator
import scala.collection.mutable
import scala.io.Codec

final class Lexer(
    path: os.Path,
    src: BufferedIterator[Char]
)(using reporter: Reporter)
    extends Iterable[Token]:
  private[this] var line = 1
  private[this] var column = 1

  val tokens: LazyList[Token] = mkTokens()
  def iterator: Iterator[Token] = tokens.iterator

  inline private def pos = Position.InFile(line, column, path)

  private def peek: Option[Char] = src.headOption
  private def peek(char: Char): Boolean =
    val result = peek.contains(char)
    if result then advance()
    result

  private def advance(): Char =
    assert(src.hasNext)
    val next = src.next()
    next match
      case '\n' =>
        line += 1
        column = 1
      case '\t' =>
        column += 4
      case ' ' =>
        column += 1
      case _ => ()
    next

  private def mkTokens(): LazyList[Token] =
    if !src.hasNext then Token(Token.Eof, "<eof>", pos) #:: LazyList.empty
    else
      advance() match
        case w if w.isWhitespace => mkTokens()
        case '(' => Token(Token.OpenParen, "(", pos) #:: mkTokens()
        case ')' => Token(Token.CloseParen, ")", pos) #:: mkTokens()
        case '[' => Token(Token.OpenBracket, "[", pos) #:: mkTokens()
        case ']' => Token(Token.CloseBracket, "]", pos) #:: mkTokens()
        case '{' => Token(Token.OpenBrace, "{", pos) #:: mkTokens()
        case '}' => Token(Token.CloseBrace, "}", pos) #:: mkTokens()
        case ':' => Token(Token.Colon, ":", pos) #:: mkTokens()
        case '.' => Token(Token.Dot, ":", pos) #:: mkTokens()
        case ';' => Token(Token.Semi, ":", pos) #:: mkTokens()
        case ',' => Token(Token.Comma, ",", pos) #:: mkTokens()
        case '"' => finishString() #:: mkTokens()
        // case '\'' =>
        //   val token @ Token(_, contents, pos) = finishString()
        //   val result = if peek('\'') then
        //     if contents.length != 1 then lexicalError(Lexer.LongCharLiteral(contents, pos))
        //     token.copy(kind = Token.Char)
        //   else
        //     token.copy(kind = Token.LowerName)
        //   result #:: mkTokens()
        case '<' if peek('-') =>
          Token(Token.LeftArrow, "<-", pos) #:: mkTokens()
        case '<' if peek('=') =>
          Token(Token.LessThanOrEql, "<=", pos) #:: mkTokens()
        case '>' if peek('=') =>
          Token(Token.GreaterThanOrEql, ">=", pos) #:: mkTokens()
        case '-' if peek('>') =>
          Token(Token.ThinArrow, "->", pos) #:: mkTokens()
        case '=' if peek('>') =>
          Token(Token.ThickArrow, "=>", pos) #:: mkTokens()
        case '/' if peek('*') =>
          finishComments()
          mkTokens()
        case '/' if peek('=') => Token(Token.NotEql, "/=", pos) #:: mkTokens()
        case '<'              => Token(Token.LessThan, "<", pos) #:: mkTokens()
        case '>' => Token(Token.GreaterThan, ">", pos) #:: mkTokens()
        case '=' => Token(Token.Eql, "=", pos) #:: mkTokens()
        case '/' => Token(Token.Div, "/", pos) #:: mkTokens()
        case '+' => Token(Token.Add, "+", pos) #:: mkTokens()
        case '-' => Token(Token.Sub, "-", pos) #:: mkTokens()
        case '*' => Token(Token.Mul, "*", pos) #:: mkTokens()

        case v if v.isLetter || v == '_' => finishIdentifier(v) #:: mkTokens()
        // case '0' if peek('b')            => ???
        // case '0' if peek('x')            => ???
        case d if d.isDigit => finishNumber(d) #:: mkTokens()
        case c =>
          lexicalError(Lexer.Unknown(c, pos))
          Token(Token.Unknown, c.toString, pos) #:: mkTokens()

  // TODO: lexer error handling
  private def lexicalError(err: Lexer.Error): Unit =
    reporter.error(err)

  private def finishComments(): Unit =
    var done = false
    val startPos = pos.copy(column = column - 2)

    while !done && src.hasNext do
      val next = advance()
      if next == '/' && peek('*') then finishComments()
      else if next == '*' && peek('/') then done = true

    if !done then lexicalError(Lexer.Unclosed("Comment", startPos))

  private def finishNumber(start: Char): Token =
    val startPos = pos.copy(column = column - 1)
    val builder = mutable.StringBuilder()
    builder += start

    while src.hasNext && (src.head.isDigit || src.head == '_') do
      val next = advance()
      if next != '_' then builder += next

    val result = builder.toString

    try result.toInt
    catch {
      case e: NumberFormatException =>
        lexicalError(Lexer.InvalidIntLiteral(result, startPos))
    }

    Token(Token.Int, builder.toString, startPos)

  private def finishString(terminator: Char = '"'): Token =
    val startPos = pos.copy(column = column - 1)
    val builder = mutable.StringBuilder()

    while src.hasNext && src.head != terminator do
      // TODO: Handle escape sequences
      val next = advance()
      builder += next

    if !peek(terminator) then lexicalError(Lexer.Unclosed("String", startPos))
    Token(Token.String, builder.toString, startPos)

  private def finishIdentifier(start: Char): Token =
    val startPos = pos.copy(column = column - 1)
    val builder = mutable.StringBuilder()
    builder += start
    while src.hasNext && (src.head.isLetterOrDigit || src.head == '_') do
      builder += advance()
    val identifier = builder.toString

    Lexer.Keywords
      .get(identifier)
      .map(Token(_, identifier, startPos))
      .getOrElse {
        val kind =
          if identifier.head.isUpper then Token.UpperName else Token.LowerName
        Token(kind, identifier, startPos)
      }

object Lexer:
  def apply(in: Path)(using reporter: Reporter): BufferedIterator[Token] =
    val charstream = os.read(in, Codec.UTF8)
    val lexer = new Lexer(in, charstream.iterator.buffered)(using reporter)

    // if ctx.dumpTokens then
    //   for token <- lexer.tokens.iterator do
    //     val pos = s"[${token.pos.render}]:".padTo(49, ' ')
    //     val kind = token.kind.toString.padTo(16, ' ')
    //     val colored = token.kind.color(s"${kind} ${token.literal}")
    //     println(s"$pos $colored")

    lexer.iterator.buffered

  sealed abstract class Error
  case class Unknown(char: Char, pos: Position) extends Error
  case class Unclosed(tpe: "Comment" | "String", pos: Position) extends Error
  case class LongCharLiteral(literal: String, pos: Position) extends Error
  case class InvalidIntLiteral(literal: String, pos: Position) extends Error

  final val Keywords = Map(
    "as" -> Token.As,
    "and" -> Token.LogicalAnd,
    "or" -> Token.LogicalOr,
    "datatype" -> Token.DataType,
    "do" -> Token.Do,
    "downto" -> Token.DownTo,
    "else" -> Token.Else,
    "fn" -> Token.Fn,
    "for" -> Token.For,
    "fun" -> Token.Fun,
    "if" -> Token.If,
    "import" -> Token.Import,
    "in" -> Token.In,
    "instance" -> Token.Instance,
    "interface" -> Token.Interface,
    "let" -> Token.Let,
    "match" -> Token.Match,
    "module" -> Token.Module,
    "mut" -> Token.Mut,
    "mutual" -> Token.Mutual,
    "not" -> Token.Not,
    "then" -> Token.Then,
    "to" -> Token.To,
    "type" -> Token.Type,
    "val" -> Token.Val,
    "while" -> Token.While,
    "with" -> Token.With
  )
