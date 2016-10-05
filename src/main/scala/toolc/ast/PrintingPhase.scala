package toolc
package ast

import Trees.Program
import toolc.utils.{Context, Pipeline}

object PrintingPhase extends Pipeline[Program, Unit] {
  override def run(ctx: Context)(v: Program): Unit = {
    println(Printer(v))
  }
}
