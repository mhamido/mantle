package mhamido.mantle.util

import collection.mutable.Buffer

final case class Result[E, +A](
    result: Option[A],
    errors: Buffer[E]
) extends IterableOnce[A],
      Product,
      Serializable:

  final def map[B](f: A => B): Result[E, B] = this match
    case Result(None, errors)    => this.asInstanceOf[Result[E, B]]
    case Result(Some(x), errors) => Result(Some(f(x)), Buffer.from(errors))

  final def flatMap[B, R >: E](f: A => Result[R, B]): Result[R, B] = this match
    case Result(None, _) => this.asInstanceOf[Result[R, B]]
    case Result(Some(x), prevErrors) =>
      val Result(res, nextErrors) = f(x)
      val errors = prevErrors appendedAll nextErrors
      Result(res, errors)

  final def zip[B, R >: E](that: Result[R, B]): Result[R, (A, B)] =
    Result.combine(this, that) { case (x, y) => (x, y) }

  override def iterator: Iterator[A] = result.iterator

object Result:
  def apply[E, A](result: A): Result[E, A] =
    Result(result = Some(result), errors = Buffer.empty)

  def fatal[E](err: E): Result[E, Nothing] =
    Result(result = None, errors = Buffer(err))

  def recoverable[E](err: E): Result[E, Unit] =
    Result(result = Some(()), errors = Buffer(err))

  def combine[A, B, EA <: Err, EB <: Err, Err, Out](
      a: Result[EA, A],
      b: Result[EB, B]
  )(action: (A, B) => Out): Result[Err, Out] =
    val result = (a.result, b.result) match
      case (Some(a), Some(b)) => Some(a, b)
      case _                  => None
    val errors = Buffer[Err]()
    errors appendAll a.errors
    errors appendAll b.errors
    Result(result, errors).map(action.tupled)

  def combine[A, B, C, EA <: Err, EB <: Err, EC <: Err, Err, Out](
      a: Result[EA, A],
      b: Result[EB, B],
      c: Result[EC, C]
  )(action: (A, B, C) => Out): Result[Err, Out] =
    val result = (a.result, b.result, c.result) match
      case (Some(a), Some(b), Some(c)) => Some(a, b, c)
      case _                           => None
    val errors = Buffer[Err]()
    errors appendAll a.errors
    errors appendAll b.errors
    errors appendAll c.errors
    Result(result, errors).map(action.tupled)

  def combine[A, B, C, D, EA <: Err, EB <: Err, EC <: Err, ED <: Err, Err, Out](
      a: Result[EA, A],
      b: Result[EB, B],
      c: Result[EC, C],
      d: Result[ED, D]
  )(action: (A, B, C, D) => Out): Result[Err, Out] =
    val result = (a.result, b.result, c.result, d.result) match
      case (Some(a), Some(b), Some(c), Some(d)) => Some(a, b, c, d)
      case _                                    => None
    val errors = Buffer[Err]()
    errors appendAll a.errors
    errors appendAll b.errors
    errors appendAll c.errors
    errors appendAll d.errors
    Result(result, errors).map(action.tupled)
