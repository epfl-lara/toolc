package toolc
package utils

import java.io.File
import ast._
import ast.Trees._
import lexer.Lexer

class Frontend extends Pipeline[File, Program] {
  override def run(ctx: Context)(v: File): Program = {
    val pipe = Lexer andThen Parser
    pipe.run(ctx)(v)
  }
}
