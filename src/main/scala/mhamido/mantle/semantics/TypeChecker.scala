package mhamido.mantle.semantics

import mhamido.mantle.util.Phase
import mhamido.mantle.util.Context

class TypeChecker { 
    given TypeChecker = this
}

object TypeChecker extends Phase[Module, Module] {
  override def apply(in: Module)(using Context): Module = ???
}
