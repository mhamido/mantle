package mhamido.mantle.util

trait Phase[-A, +B] { self =>
  def apply(in: A)(using Context): B
  def andThen[C](that: Phase[B, C]): Phase[A, C] = new Phase {
    def apply(in: A)(using ctx: Context): C = {
      val result = self(in)
      ctx.reporter.exitIfErrors()
      that(result)
    }
  }
}
