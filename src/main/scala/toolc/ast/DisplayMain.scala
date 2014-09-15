package toolc
package ast

import utils._
import Trees._

object DisplayMain extends Pipeline[Program, Program] {
  def run(ctx: Context)(p: Program): Program = {
    println(p.main.id.value)
    p
  }
}
