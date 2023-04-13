package mhamido.mantle.parser

import mhamido.mantle.Token
import mhamido.mantle.syntax
import mhamido.mantle.util.Reporter
import mhamido.mantle.syntax.Decl
import scala.annotation.targetName
import mhamido.mantle.syntax.Type
import mhamido.mantle.syntax.Pattern
import mhamido.mantle.syntax.Expr

final case class ModuleParser(
    path: os.Path,
    tokens: BufferedIterator[Token]
)(using reporter: Reporter) {
  def module(): syntax.Module =
    consume(Token.Module)
    val name = qualifiedName()
    consume(Token.With)
    val defs = decls(Token.Eof)
    consume(Token.Eof)
    syntax.Module(name, defs)

  private def qualifiedName(): Seq[Name] = ???

  val Start = List(
    Token.Mutual,
    Token.Fun,
    Token.Val,
    Token.Type,
    Token.DataType
  )

  private def isAtEnd: Boolean = ???
  private def peek: Token = ???
  private def advance(): Token = ???
  private def consume(kind: Token.Kind): Token = ???

  @targetName("matchesvar")
  private def matches(tokens: Token.Kind*): Boolean = matches(tokens)
  private def matches(tokens: Seq[Token.Kind]): Boolean = ???

  def decls(terminators: Token.Kind*): Seq[Decl] =
    val defs = List.newBuilder[Decl]
    while !isAtEnd && !matches(terminators) do defs += decl(terminators*)
    defs.result()

  def decl(terminators: Token.Kind*): Decl =
    val next = advance()
    next.kind match {
      case Token.Type =>
        ???
      case Token.DataType =>
        ???
      case Token.Val =>
        ???
      case Token.Fun =>
        ???
    }

  def typeParams(): Seq[String] =
    consume(Token.OpenBracket)
    val typeParams = List.newBuilder[String]

    peek.kind match
      case Token.PrimedName =>
        typeParams += advance().literal
        while !isAtEnd && !matches(Token.CloseBracket) do
          consume(Token.Comma)
          typeParams += consume(Token.PrimedName).literal

    consume(Token.CloseBracket)
    typeParams.result()

  def typeArgs(): Seq[Type] =
    consume(Token.OpenBracket)
    val typeArgs = List.newBuilder[Type]

    while !isAtEnd && !matches(Token.CloseBracket) do
      consume(Token.Comma)
      typeArgs += tpe()

    consume(Token.CloseBracket)
    typeArgs.result()

  def tpe(): Type =
    val tpe = atomicType()
    val arrows = List.newBuilder[Type]
    while !isAtEnd && matches(Token.ThinArrow) do
      consume(Token.ThinArrow)
      arrows += atomicType()
    arrows.result().foldRight(tpe)(Type.Arrow(_, _))

  def atomicType(): Type =
    val token = advance()
    token.kind match {
      case Token.OpenBracket =>
        val paramTy = consume(Token.PrimedName).literal
        consume(Token.ThinArrow)
        val resultTy = tpe()
        ???
      case Token.OpenParen =>
        val resultTy = tpe()
        consume(Token.CloseParen)
        resultTy
      case Token.UpperName =>
        val args =
          if peek.kind == Token.OpenBracket then typeArgs()
          else List.empty

        // name?
        syntax.Type.Apply(???, args)
      case Token.PrimedName =>
        Type.Var({ token.literal; ??? })
    }

  def simplePat: Pattern =
    val token = advance()
    token.kind match {
      case _ if token.literal == "_" => Pattern.Wildcard
      case Token.LowerName           => Pattern.Var({ token.literal; ??? })
    }
  type Param = (String, Type) | String

  private inline def param(): Param =
    peek.kind match {
      case Token.OpenParen =>
        val name = consume(Token.LowerName).literal
        consume(Token.Colon)
        val annotation = tpe()
        consume(Token.CloseParen)
        (name, annotation)
      case Token.OpenBracket =>
        val name = consume(Token.PrimedName).literal
        consume(Token.CloseBracket)
        name
    }
  private inline def params(terminators: Token.Kind*): Seq[_] =
    val params = List.newBuilder[Param]
    while !isAtEnd && !matches(terminators*) do params += param()
    params.result()

  def funDecl: Decl.Function =
    val name = consume(Token.LowerName)
    val par = params(Token.Colon)
    consume(Token.Colon)
    val ret = tpe()
    consume(Token.Eql)
    val res = expr()
    ???
    // Decl.Function(name)
  def expr(): Expr =
    peek.kind match {
      case Token.Fn =>
        advance()
        val binders = params(Token.ThickArrow)
        consume(Token.ThickArrow)
        val body = expr()
        Expr.Fn(binders, body)
      case Token.If =>
        advance()
        val cond = expr()
        consume(Token.Then)
        val thenp = expr()
        consume(Token.Else)
        val elsep = expr()
        Expr.If(cond, thenp, elsep)
      case _ => ???
    }

  def atomicExpr(): Expr =
    val token = advance()
    token.kind match {
      case Token.LowerName => Expr.Var(token.literal)
      case Token.Int       => Expr.Int(token.literal.toInt)
      case Token.String    => Expr.String(token.literal)
      case Token.OpenParen => ???
      case Token.Let =>
        val defs = decls(Token.In)
        
    }
}
