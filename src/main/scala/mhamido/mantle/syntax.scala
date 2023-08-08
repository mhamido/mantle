package mhamido.mantle

import mhamido.mantle.util.Span
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

object syntax extends AbstractModule {
  type Name = String
  type Info = Span
}
