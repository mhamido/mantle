package mhamido.mantle
package util

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

case class Span(
    from: Position,
    to: Position
)

sealed trait Position:
  def line: Int
  def column: Int
  def path: os.Path

  def render: String =
    s"$path:$line:$column"

object Position:
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
