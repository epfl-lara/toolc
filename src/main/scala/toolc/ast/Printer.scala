package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    var _ident: Int = 0
    val sb = new StringBuffer()
    def app(s: String) = sb append s
    def line(s: String) = {
      sb append ident
      sb append s
      sb append "\n"
    }
    def nl() = sb append "\n"
    def ident: String = "  " * _ident
    def paren(body: => Unit): Unit = {
      app("(")
      body
      app(")")
    }

    def indented(body: => Unit) = {
      _ident += 1
      body
      _ident -= 1
    }

    def binOp(lhs: Tree, op: String, rhs: Tree) = {
      paren {
        toStr(lhs)
        app(s" $op ")
        toStr(rhs)
      }
    }

    def toStr(t: Tree): Unit = {
      t match {
        case Program(mainObject, classes) =>
          toStr(mainObject)
          nl()
          nl()
          classes foreach { cl => toStr(cl); nl() }
        case MainObject(id, stats) =>
          line(s"object $id {")
          indented {
            line("def main(): Unit = {")
            indented {
              stats foreach { st => toStr(st); nl() }
            }
            nl()
            line("}")
          }
          line("}")

        case ClassDecl(id, parent, vars, methods) =>
          val optP = parent match {
            case Some(idp) => s" extends $idp"
            case None => ""
          }
          line(s"class $id$optP {")
          indented {
            vars map { v => toStr(v); nl() }
            nl()
            methods map { m => toStr(m); nl() }
          }
          line("}")

        case Formal(tpe, id) =>
          toStr(id); app(": "); toStr(tpe)

        case VarDecl(tpe, id) =>
          app(s"var $id: "); toStr(tpe); app(";")

        case MethodDecl(retType, id, args, vars, stats, retExpr) =>
          app(s"def $id(")
          if (args.nonEmpty) {
            args.init foreach { arg => toStr(arg); app(", ") }
            toStr(args.last)
          }
          app("): ")
          toStr(retType)
          app(" = {\n")
          indented{
            vars foreach {v => toStr(v); nl()}
            nl()
            stats foreach {s => toStr(s); nl()}
            app("return ")
            toStr(retExpr)
            nl()
          }

        case IntArrayType() => app("Int[]")
        case IntType()      => app("Int")
        case BooleanType()  => app("Bool")
        case StringType()   => app("String")
        case ClassType(id)  => app(id.value)

        case Block(stats) =>
          app("{\n")
          indented {
            stats foreach {s => toStr(s); nl()}
          }
          line("}")

        case While(expr, stat) =>
          app("while (")
          toStr(expr)
          app(") ")
          toStr(stat)

        case If(cond, thn, Some(els)) =>
          app("if (")
          toStr(cond)
          app(") ")
          toStr(thn)
          app(" else ")
          toStr(els)

        case If(cond, thn, None) =>
          app("if (")
          toStr(cond)
          app(") ")
          toStr(thn)

        case Println(expr) =>
          app("println(")
          toStr(expr)
          app(");")
        case Assign(id, expr) =>
          toStr(id)
          app(" = ")
          toStr(expr)
          app(";")
        case ArrayAssign(id, index, expr) =>
          toStr(id)
          app("[")
          toStr(index)
          app("] = ")
          toStr(expr)
          app(";")
        case DoExpr(expr) =>
          app("do(")
          toStr(expr)
          app(");")
        case And(lhs, rhs)      => binOp(lhs, "&&", rhs)
        case Or(lhs, rhs)       => binOp(lhs, "||", rhs)
        case Plus(lhs, rhs)     => binOp(lhs, "+", rhs)
        case Minus(lhs, rhs)    => binOp(lhs, "-", rhs)
        case Times(lhs, rhs)    => binOp(lhs, "*", rhs)
        case Div(lhs, rhs)      => binOp(lhs, "/", rhs)
        case LessThan(lhs, rhs) => binOp(lhs, "<", rhs)
        case Equals(lhs, rhs)   => binOp(lhs, "==", rhs)

        case ArrayRead(arr, index) => paren {
          toStr(arr)
          app("[")
          toStr(index)
          app("]")
        }
        case ArrayLength(arr) =>
          paren(toStr(arr))
          app(".length")
        case MethodCall(obj, meth, args) =>
          toStr(obj)
          app(".")
          toStr(meth)
          app("(")
          if (args.nonEmpty) {
            args.init.foreach { arg =>
              toStr(arg)
              app(", ")
            }
            toStr(args.last)
          }
          app(")")
        case IntLit(value) => app(value.toString)
        case StringLit(value) => app("\"" + value + "\"")
        case True() => app("true")
        case False() => app("false")
        case id@Identifier(value) => app(value)
        case This() => app("this")
        case NewIntArray(size) =>
          app("new Int[")
          toStr(size)
          app("]")
        case New(tpe) =>
          app("new ")
          toStr(tpe)
          app("()")
        case Not(expr) =>
          app("!")
          paren(toStr(expr))
        case Variable(id) =>
          app(id.value)
      }
    }

    toStr(t)
    sb.toString
  }
}
