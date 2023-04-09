import scala.util.Try
import mhamido.mantle.syntax.Pattern
import mhamido.mantle.syntax.Name
import mhamido.mantle.syntax.parser.ModuleParser

class PatternParserSuite extends munit.FunSuite {
  private given Conversion[String, Name] = {
    case s if s.head.isUpper => Name.Constructor(s)
    case s if s.head.isLower => Name.Var(s)
  }

  private given Conversion[String, Pattern] = Pattern.Var(_)
  private given Conversion[Int, Pattern] = Pattern.Int(_)

  def check[T](
      pattern: String,
      expected: Pattern
  )(using loc: munit.Location): Unit =
    test(s"pattern \"$pattern\" parses properly") {
      val (parser, _) = ModuleParser.debug(pattern)
      val result = parser.pattern()
      assertEquals(result, expected)
    }

  def fail[T](
      pattern: String
  )(using loc: munit.Location): Unit =
    test(s"pattern \"$pattern\" parses properly".fail) {
      val (parser, reporter) = ModuleParser.debug(pattern)
      val result = parser.pattern()
      assert(!reporter.hadErrors)
    }

  check("foo", Pattern.Var("foo"))
  check("Bar", Pattern.Constructor("Bar", Seq()))
  check("Foo bar", Pattern.Constructor("Foo", List(Pattern.Var("bar"))))
  check(
    "Foo bar baz",
    Pattern.Constructor("Foo", List(Pattern.Var("bar"), Pattern.Var("baz")))
  )
  // "x: Int",
  check("foo as bar", Pattern.Alias(Pattern.Var("foo"), "bar"))
  check(
    "(x, y, z)",
    Pattern.Tuple(List(Pattern.Var("x"), Pattern.Var("y"), Pattern.Var("z")))
  )

  check(
    "(0 as x, 0 as y, \"foo\")",
    Pattern.Tuple(
      List(Pattern.Alias(0, "x"), Pattern.Alias(0, "y"), Pattern.String("foo"))
    )
  )
  // "(x: Int, y: Int, z: Int)",
  check(
    "Some (x, y, z)",
    Pattern.Constructor(
      "Some",
      List(
        Pattern.Tuple(
          List(Pattern.Var("x"), Pattern.Var("y"), Pattern.Var("z"))
        )
      )
    )
  )

  check(
    "(Some x)",
    Pattern.Constructor("Some", List(Pattern.Var("x")))
  )

  check(
    "(Foo x y z, Bar, q as uwu)",
    Pattern.Tuple(
      List(
        Pattern.Constructor("Foo", Seq("x", "y", "z")),
        Pattern.Constructor("Bar", List()),
        Pattern.Alias("q", "uwu")
      )
    )
  )

  // "Some (x as foo, y as bar, z as baz)"
  fail("as foo")
  fail("var as 1")
  fail("_ as 10")

}
