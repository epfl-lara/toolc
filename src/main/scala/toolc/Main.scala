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

      case "--tree" :: args =>
        ctx = ctx.copy(printTree = true)
        processOption(args)

      case "--symbols" :: args =>
        ctx = ctx.copy(printSymbols = true)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f :: args =>
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
    val msg = """|Usage: ./toolc [options] <file>
                 |Options include:
                 |  --help        displays this help
                 |  --tokens      displays the list of tokens
                 |  --tree        displays the parsed tree
                 |  --symbols     displays the parsed tree after symbol analysis
                 |  --eval        evaluate the program directly instead of generating code
                 |  -d <outdir>   generates class files in the specified directory"""
    println(msg.stripMargin)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val pipeline = if (ctx.doTokens) {
      Lexer andThen DisplayTokens
    } else {
      Lexer andThen
      Parser andThen
        (if (ctx.printTree) {
          new PrintTree(false)
        } else if (ctx.printSymbols) {
          NameAnalysis andThen new PrintTree(true)
        } else {
          NameAnalysis andThen
          TypeChecking andThen
          (if (ctx.doEval) {
            Evaluation
          } else {
            CodeGeneration
          })
        })
    }

    pipeline.run(ctx)(ctx.files.head)

    ctx.reporter.terminateIfErrors()
  }
}
