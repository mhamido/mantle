package mhamido.mantle
package syntax

final case class Module(
    name: Seq[Name],
    // imports: Seq[Import],
    decls: Seq[Decl]
)
