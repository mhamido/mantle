package mhamido.mantle

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import fansi.Color

import mhamido.mantle.util.Position
import mhamido.mantle.syntax.Name

case class Token(
    kind: Token.Kind,
    literal: String,
    pos: Position
):
  def toName: Name = literal.head match
    case '\''           => Name.TypeVar(literal.tail)
    case u if u.isUpper => Name.Constructor(literal)
    case _              => Name.Var(literal)

  def render: String = s"\"${literal}\" (${kind.render})"

object Token:
  export Kind.*

  given JsonValueCodec[Token] = JsonCodecMaker.make
  given JsonValueCodec[Kind] = JsonCodecMaker.make

  enum Kind extends Enum[Kind]:
    case Eof, Unknown

    case LowerName, UpperName, PrimedName, OperatorName
    case String, Char, Int

    case Eql, NotEql
    case Not, LogicalAnd, LogicalOr
    case Add, Sub, Mul, Div
    case LessThan, GreaterThan, LessThanOrEql, GreaterThanOrEql

    case Let, Fun, Val, In
    case Type, DataType, Mutual
    case Module, Import, With, Interface, Instance
    case For, While, DownTo, To, Do, Mut
    case If, Then, Else, As, Fn
    case Match

    case Dot, Semi, Colon, Comma
    case ThinArrow, ThickArrow, LeftArrow

    case OpenParen, CloseParen
    case OpenBracket, CloseBracket
    case OpenBrace, CloseBrace

    def color = this match
      case Unknown                  => Color.Red
      case Eof                      => Color.DarkGray
      case UpperName                => Color.Cyan
      case PrimedName               => Color.Blue
      case LowerName | OperatorName => Color.LightBlue
      case String | Char | Int      => Color.LightGreen
      case OpenParen | CloseParen | OpenBracket | CloseBracket | OpenBrace |
          CloseBrace =>
        Color.Yellow
      case _ => Color.LightGray

    def render: String = this match
      case Eof              => "<eof>"
      case Unknown          => "<unknown>"
      case LowerName        => "<varname>"
      case UpperName        => "<conname>"
      case PrimedName       => "<typename>"
      case OperatorName     => "<opname>"
      case String           => "<string>"
      case Char             => "<char>"
      case Int              => "<int>"
      case Eql              => "="
      case NotEql           => "/="
      case Not              => "not"
      case LogicalAnd       => "&&"
      case LogicalOr        => "||"
      case Add              => "+"
      case Sub              => "-"
      case Mul              => "*"
      case Div              => "/"
      case LessThan         => "<"
      case GreaterThan      => ">"
      case LessThanOrEql    => "<="
      case GreaterThanOrEql => ">="
      case Let              => "let"
      case Fun              => "fun"
      case Val              => "val"
      case In               => "in"
      case Type             => "type"
      case DataType         => "datatype"
      case Mutual           => "mutual"
      case Module           => "module"
      case Import           => "import"
      case With             => "with"
      case Interface        => "interface"
      case Instance         => "instance"
      case For              => "for"
      case While            => "while"
      case DownTo           => "downto"
      case To               => "to"
      case Do               => "do"
      case Mut              => "mut"
      case If               => "if"
      case Then             => "then"
      case Else             => "else"
      case As               => "as"
      case Fn               => "fn"
      case Match            => "match"
      case Dot              => "."
      case Semi             => ";"
      case Colon            => ":"
      case Comma            => ","
      case ThinArrow        => "->"
      case ThickArrow       => "=>"
      case LeftArrow        => "<-"
      case OpenParen        => "("
      case CloseParen       => ")"
      case OpenBracket      => "["
      case CloseBracket     => "]"
      case OpenBrace        => "{"
      case CloseBrace       => "}"
