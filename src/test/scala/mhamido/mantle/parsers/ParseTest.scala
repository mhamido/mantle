package mhamido.mantle.language.parsers

import os._
import mhamido.SnapshotSuite
import mhamido.mantle.parsing.ModuleParser

class ParseTest extends SnapshotSuite("parser") {
    val dir = os.list(os.pwd / "examples")
    for file <- dir do
        snapshot(file) {
            val src = os.read(file)
            ModuleParser(file).get
        }
}
