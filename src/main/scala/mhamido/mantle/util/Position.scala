package mhamido.mantle
package util

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

case class Span private (from: Position, to: Position) {
  def <>(that: Span): Span = {
    val start = Ordering[Position].min(from, that.from)
    val finish = Ordering[Position].max(to, that.to)
    Span(start, finish)
  }

  def <>(that: Position): Span = new Span(from, Ordering[Position].max(that, to))
}

object Span {
  def apply(from: Position, to: Position): Span = {
    val start = Ordering[Position].min(from, to)
    val finish = Ordering[Position].max(from, to)
    new Span(start, finish)
  }

  given Ordering[Span] = Ordering.by[Span, Position](_.from).orElseBy(_.to)
}

sealed trait Position:
  def line: Int
  def column: Int
  def path: os.Path

  def <>(that: Position): Span = Span(this, that)
  def <>(that: Span): Span = Span(this, that.to)
  // def to(that: Seq[Span | Position]) = ???

  def render: String =
    s"$path:$line:$column"

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
    def line: Int = -1
    def column: Int = -1
    def path: os.Path = os.temp()

  case class InFile(
      line: Int,
      column: Int,
      path: os.Path
  ) extends Position
