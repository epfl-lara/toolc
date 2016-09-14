package toolc

import lexer._
import ast._
import ast.Trees._
import utils._

object ParserHarness {
  def main(args: Array[String]) {
    val ctx = Main.processOptions(args)

    val pipeline = Lexer  andThen Parser andThen SafePrinter

    pipeline.run(ctx)(ctx.files.head)

    ctx.reporter.terminateIfErrors()
  }

  object SafePrinter extends Pipeline[Program, Unit] {
    def run(ctx: Context)(prog: Program): Unit = {
      println(apply(prog))
    }

    def apply(t: Tree): String = {
      var _ident: Int = 0

      def incIdent = { _ident = _ident + 1 }
      def decIdent = { _ident = _ident - 1 }
      def ident: String = (0 to _ident).toList.tail.map(s => "  ").mkString("")
      def paren(str: String): String = "(" + str + ")"

      def toStr(t: Tree): String = {
        t match {
          case Program(mainObject, classes) => toStr(mainObject) + "\n\n" + classes.map(toStr(_)).mkString("\n\n")
          case MainObject(id, stats) =>
            var str = ident + "object " + toStr(id) + " {\n"
            incIdent
            str = str + ident + "def main() : Unit = {\n"
            incIdent
            str = str + ident + stats.map(toStr(_)).mkString("\n"+ident) + "\n"
            decIdent
            str = str + "\n" + ident + "}\n"
            decIdent
            str = str + ident + "}"
            str

          case ClassDecl(id, parent, vars, methods) =>
            var str = ident + "class " + toStr(id) + " "
            parent match {
              case Some(idp) => str = str + "extends " + toStr(idp) + " "
              case None => ;
            }
            str = str + "{\n"
            incIdent
            if(!vars.isEmpty)
              str = str + ident + vars.map(toStr(_)).mkString("\n"+ident) + "\n"
            if(!methods.isEmpty)
              str = str + ident + methods.map(toStr(_)).mkString("\n"+ident) + "\n"
            decIdent
            str = str + ident + "}"
            str

          case Formal(tpe, id) => toStr(id) + ": " + toStr(tpe)

          case VarDecl(tpe, id) => "var " + toStr(id) + ": " + toStr(tpe) + ";"

          case MethodDecl(retType, id, args, vars, stats, retExpr) =>
            var str = "def " + toStr(id) + "(" + args.map(f => { toStr(f.id) + " : " + toStr(f.tpe) }).mkString(", ") + ") : " + toStr(retType) + " = {\n"
            incIdent
            str = str + ident + vars.map(toStr(_)).mkString("\n"+ident) + "\n"
            str = str + ident + stats.map(toStr(_)).mkString("\n"+ident) + "\n"
            str = str + ident + "return " + toStr(retExpr) + ";\n"
            decIdent
            str = str + ident + "}"
            str

          case id @ Identifier(value) => value

          case IntArrayType() => "Int[]"
          case IntType() => "Int"
          case BooleanType() => "Bool"
          case StringType() => "String"
          case ClassType(id) => toStr(id)
          
          case Block(lst) =>
            val i2 = ident
            incIdent
            val i = ident
            val ret = "{\n" + i + lst.map(toStr(_)).mkString("\n" + i) + "\n" + i2 + "}"
            decIdent
            ret
          
          case While(expr, stat) => "while (" + toStr(expr) + ") " + toStr(stat)
          
          case If(cond, thn, Some(els)) =>
            "if (" + toStr(cond) + ") " + toStr(thn) + " else " + toStr(els)

          case If(cond, thn, None) =>
            "if (" + toStr(cond) + ") " + toStr(thn)
          
          case Println(expr) => "println(" + toStr(expr) + ");"
          case Assign(id, expr) => toStr(id) + " = " + toStr(expr) + ";"
          case ArrayAssign(id, index, expr) => toStr(id) + "[" + toStr(index) + "] = " + toStr(expr) + ";"
          case And(lhs, rhs) => paren(toStr(lhs) + " && " + toStr(rhs))
          case Or(lhs, rhs) => paren(toStr(lhs) + " || " + toStr(rhs))
          case Plus(lhs, rhs) => paren(toStr(lhs) + " + " + toStr(rhs))
          case Minus(lhs, rhs) => paren(toStr(lhs) + " - " + toStr(rhs))
          case Times(lhs, rhs) => paren(toStr(lhs) + " * " + toStr(rhs))
          case Div(lhs, rhs) => paren(toStr(lhs) + " / " + toStr(rhs))
          case LessThan(lhs, rhs) => paren(toStr(lhs) + " < " + toStr(rhs))
          case Equals(lhs, rhs) => paren(toStr(lhs) + " == " + toStr(rhs))
          case ArrayRead(arr, index) => paren(toStr(arr) + "[" + toStr(index) + "]")
          case ArrayLength(arr) => paren(toStr(arr)) + ".length"
          case MethodCall(obj, meth, args) => paren(toStr(obj) + "." + toStr(meth) + "(" + args.map(toStr(_)).mkString(", ") + ")")
          case IntLit(value) => value.toString
          case StringLit(value) => "\"" + value + "\""
          case True() => "true"
          case False() => "false"
          case Variable(id) => toStr(id)
          case This() => "this"
          case NewIntArray(size) => "new Int[" + toStr(size) + "]"
          case New(tpe) => "new " + toStr(tpe) + "()"
          case Not(expr) => "!" + paren(toStr(expr))
        }
      }

      toStr(t)
    }
  }

}
