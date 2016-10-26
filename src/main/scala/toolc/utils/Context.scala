package toolc
package utils

import java.io.File

case class Context(
  val reporter: Reporter,
  val files: List[File] = Nil,
  val outDir: Option[File] = None,
  val doEval: Boolean = false,
  val doHelp: Boolean = false,
  val doTokens: Boolean = false,
  val printTree: Boolean = false,
  val printSymbols: Boolean = false
)
