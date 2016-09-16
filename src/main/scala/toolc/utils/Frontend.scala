package toolc
package utils

import java.io.File
import ast._
import ast.Trees._
import toolc.analyzer.{TypeChecking, NameAnalysis}
import toolc.lexer.Lexer

/**
  * Created by koukouto on 9/16/16.
  */
object Frontend extends Pipeline[File, Program] {
  override def run(ctx: Context)(v: File): Program = {
    val pipe = Lexer andThen Parser andThen NameAnalysis andThen TypeChecking
    pipe.run(ctx)(v)
  }
}
