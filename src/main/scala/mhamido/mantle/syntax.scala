package mhamido.mantle

import mhamido.mantle.util.Span
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

object syntax extends AbstractModule {
  type Info = Span
  type Name = String
}
