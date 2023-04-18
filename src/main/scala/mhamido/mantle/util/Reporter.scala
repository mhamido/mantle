package mhamido.mantle.util

import fansi.Color

/** https://github.com/epfl-lara/toolc/blob/master/src/main/scala/toolc/utils/Reporter.scala
  */

class Reporter:
  private var _hadErrors = false

  def hadErrors: Boolean = _hadErrors

  def info[A](msg: A, pos: Position = Position.Empty): Unit =
    report(Reporter.Info, msg, pos)

  def error[A](msg: A, pos: Position = Position.Empty): Unit =
    _hadErrors = true
    report(Reporter.Error, msg, pos)

  def warning[A](msg: A, pos: Position = Position.Empty): Unit =
    report(Reporter.Warning, msg, pos)

  inline def fatalError[A <: Exception](msg: A, pos: Position = Position.Empty): Nothing =
    report(Reporter.Error, msg, pos)
    throw msg

  def exitIfErrors(): Unit = if hadErrors then sys.exit(1)

  protected def report[A](prefix: String, msg: A, pos: Position): Unit =
    pos match
      case Position.Empty => Console.err.println(s"$prefix: $msg")
      case Position.InFile(line, column, path) =>
        Console.err.println(s"${pos.render}: $prefix: $msg")

object Reporter:
  final val Info = Color.Cyan("Info").render
  final val Error = Color.Red("Error").render
  final val Warning = Color.Yellow("Warning").render
