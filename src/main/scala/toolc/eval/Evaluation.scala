package toolc
package eval

import ast.Trees._
import utils._

object Evaluation extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    val evaluator = new Evaluator(ctx, prog)
    evaluator.eval()
  }

}
