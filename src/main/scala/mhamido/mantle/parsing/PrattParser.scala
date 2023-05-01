package mhamido.mantle.parsing

abstract class PrattParser[A] {
    type Prefix = Token => A
    type Infix = (A, Token) => A

    def prim(token: Token): A
    def infixes: Map[Token.Kind, Infix]
    def prefixes: PartialFunction[Token.Kind, Prefix]
}

object PrattParser {
}