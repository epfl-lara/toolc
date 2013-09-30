package toolc
package ast

import Trees._

object ASTPrinter {
  def apply(t: Tree): String = {
    var _ident: Int = 0

    def paren(str: String): String = "(" + str + ")"

    def toStr(t: Tree): String = {
      t match {
        case Identifier(v) => "Identifier(\""+v+"\")"
        case _ => t.toString
      }
    }

    toStr(t)
  }
}
