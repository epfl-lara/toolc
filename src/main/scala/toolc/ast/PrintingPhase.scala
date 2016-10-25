package toolc
package ast

import Trees.Program
import toolc.utils.{Context, Pipeline}

class PrintingPhase(unique: Boolean) extends Pipeline[Program, Unit] {
  override def run(ctx: Context)(v: Program): Unit = {
    println(Printer(v, unique))
  }
}
