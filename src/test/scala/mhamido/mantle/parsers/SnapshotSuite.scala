package mhamido

import munit._
import os.Path

import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonValueCodec,
  JsonWriter,
  JsonReader
}
import com.github.plokhotnyuk.jsoniter_scala.core

import scala.util.Using
import munit.internal.console.StackTraces
import com.github.plokhotnyuk.jsoniter_scala.core.ReaderConfig
import mhamido.mantle.util.Reporter

// Mainly inspired by: https://github.com/lolgab/munit-snapshot
// But saves tests on a component & file-by-file basis rather than stuffing everything into
// a single JSON file.

trait SnapshotSuite(componentName: String) extends munit.FunSuite:
  given reporter: Reporter = new Reporter {}
  protected final val indent = 2

  def snapshot[A](
      path: Path
  )(body: => A)(using codec: JsonValueCodec[A])(using loc: Location): Unit =
    test(path.toString) {
      val root = os.Path(loc.path) / os.up
      val component = root / componentName
      if !os.exists(component) then os.makeDir(component)
      val outpath = component / s"${path.last}.json"

      StackTraces.dropInside {
        // First run of the test, run the body and cache the results to disk.
        if !os.exists(outpath) then
          Using.resource(os.write.outputStream(outpath)) { outstream =>
            core.writeToStream(body, outstream)
          }
        // Load the previous results off disk and compare the current output
        else
          val previous = Using.resource(os.read.inputStream(outpath)) {
            instream =>
              core.readFromStream(instream)
          }
          val current = body
          assertEquals(current, previous)
      }
    }
