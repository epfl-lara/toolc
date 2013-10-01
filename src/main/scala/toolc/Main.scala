package toolc

import utils._
import java.io.File

import lexer._
import ast._
import analyzer._
import code._
import eval._

object Main {

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var ctx = Context(reporter = reporter)

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "--eval" :: args =>
        ctx = ctx.copy(doEval = true)
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f ::args =>
        ctx = ctx.copy(files = new File(f) :: ctx.files)
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }
    if (ctx.files.size != 1) {
      reporter.fatal("Exactly one file expected, "+ctx.files.size+" file(s) given.")
    }

    ctx
  }

  def displayHelp() {
    println("Usage: ./toolc [options] <file>")
    println("Options include:")
    println(" --help        display this help")
    println(" --tokens      display the list of tokens")
    println(" --eval        evaluate the program directly instead of generating code")
    println(" -d <outdir>   generate class files in the specified directory")
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipeline = {
      Lexer andThen
      (if (ctx.doTokens) {
        DisplayTokens
      } else {
        Noop()
      }) andThen
      Parser andThen
      NameAnalysis andThen
      TypeChecking andThen
      (if (ctx.doEval) {
        Evaluation
      } else {
        CodeGeneration
      })
    }

    pipeline.run(ctx)(ctx.files.head)
  }
}
