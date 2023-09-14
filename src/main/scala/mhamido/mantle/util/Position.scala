package mhamido.mantle.util

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

// todo: convert offsets into line/col positions
case class Span(startOffset: Int, endOffset: Int):
  require(startOffset <= endOffset)
  def length: Int = endOffset - startOffset
  def <>(that: Span): Span = Span(
    startOffset min that.startOffset,
    endOffset max that.endOffset
  )

object Span:
  given Ordering[Span] =
    Ordering.by[Span, Int](_.startOffset).orElseBy(_.endOffset)

@deprecated
sealed trait Position:
  def line: Int
  def column: Int
  def path: os.Path

  def <>(that: Position): Span = ???
  def <>(that: Span): Span     = ???
  // def to(that: Seq[Span | Position]) = ???

  def render: String =
    s"$path:$line:$column"

@deprecated
object Position:
  given Ordering[Position] =
    Ordering.by[Position, Int](_.line).orElseBy(_.column)

  given JsonValueCodec[Position] = JsonCodecMaker.make
  given JsonValueCodec[os.Path] = new JsonValueCodec[os.Path]:
    override def decodeValue(in: JsonReader, default: os.Path): os.Path =
      os.Path(in.readString(default.toString))

    override def encodeValue(x: os.Path, out: JsonWriter): Unit =
      out.writeVal(x.toString)

    override def nullValue: os.Path = os.temp()

  case object Empty extends Position:
    def line: Int     = -1
    def column: Int   = -1
    def path: os.Path = os.temp()

  case class InFile(
      line: Int,
      column: Int,
      path: os.Path
  ) extends Position
