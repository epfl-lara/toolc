package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import java.io.File

object ASTDumpParser extends Pipeline[ASTNode, Program] {

  def run(ctx: Context)(n: ASTNode): Program = {
    x[Program](n)
  }

  def xList[T <: Tree](t: ASTTree): List[T] = t match {
    case l: ASTList =>
      l.l.asInstanceOf[Seq[ASTNode]].map(x[T](_)).toList
    case _ =>
      sys.error("Woot")
  }

  def xOpt[T <: Tree](t: ASTTree): Option[T] = t match {
    case o: ASTOption =>
      o.o.asInstanceOf[Option[ASTNode]].map(x[T](_))
    case _ =>
      sys.error("Woot")
  }

  def x[T <: Tree](t: ASTTree): T = t match {
    case n: ASTNode =>
      val res = n match {
        case Node("Program", subs) =>
          Program(x(subs(0)), xList(subs(1)))
        case Node("MainObject", subs) =>
          MainObject(x(subs(0)), xList(subs(1)))
        case Node("ClassDecl", subs) =>
          ClassDecl(x(subs(0)), xOpt(subs(1)), xList(subs(2)), xList(subs(3)))
        case Node("VarDecl", subs) =>
          VarDecl(x(subs(0)), x(subs(1)))
        case Node("MethodDecl", subs) =>
          MethodDecl(x(subs(0)), x(subs(1)), xList(subs(2)), xList(subs(3)), xList(subs(4)), x(subs(5)))
        case Node("Formal", subs) =>
          Formal(x(subs(0)), x(subs(1)))
        case Node("IntArrayType", Nil) =>
          IntArrayType()
        case Node("IntType", Nil) =>
          IntType()
        case Node("BooleanType", Nil) =>
          BooleanType()
        case Node("StringType", Nil) =>
          StringType()
        case Node("Block", subs) =>
          Block(xList(subs(0)))
        case Node("If", subs) =>
          If(x(subs(0)), x(subs(1)), xOpt(subs(2)))
        case Node("While", subs) =>
          While(x(subs(0)), x(subs(1)))
        case Node("Println", subs) =>
          Println(x(subs(0)))
        case Node("Assign", subs) =>
          Assign(x(subs(0)), x(subs(1)))
        case Node("ArrayAssign", subs) =>
          ArrayAssign(x(subs(0)), x(subs(1)), x(subs(2)))

        case Node("And", subs) =>
          And(x(subs(0)), x(subs(1)))
        case Node("Or", subs) =>
          Or(x(subs(0)), x(subs(1)))
        case Node("Plus", subs) =>
          Plus(x(subs(0)), x(subs(1)))
        case Node("Minus", subs) =>
          Minus(x(subs(0)), x(subs(1)))
        case Node("Times", subs) =>
          Times(x(subs(0)), x(subs(1)))
        case Node("Div", subs) =>
          Div(x(subs(0)), x(subs(1)))
        case Node("LessThan", subs) =>
          LessThan(x(subs(0)), x(subs(1)))
        case Node("Equals", subs) =>
          Equals(x(subs(0)), x(subs(1)))
        case Node("ArrayRead", subs) =>
          ArrayRead(x(subs(0)), x(subs(1)))
        case Node("ArrayLength", subs) =>
          ArrayLength(x(subs(0)))

        case Node("MethodCall", subs) =>
          MethodCall(x(subs(0)), x(subs(1)), xList(subs(2)))

        case Node("NewIntArray", subs) =>
          NewIntArray(x(subs(0)))
        case Node("New", subs) =>
          New(x(subs(0)))
        case Node("Not", subs) =>
          Not(x(subs(0)))
        case Node("This", Nil) =>
          This()
        case Node("True", Nil) =>
          True()
        case Node("False", Nil) =>
          False()

        case Lit("IntLit", v: Int) =>
          IntLit(v)
        case Lit("StringLit", v: String) =>
          StringLit(v)
        case Lit("Identifier", v: String) =>
          Identifier(v)

          
        case n: ASTNode =>
          println("Woot ?: "+n.kind)
      }
      res.asInstanceOf[T].setPos(posToPos(n.p))

    case _ =>
      sys.error("Woot")
  }

  object Node {
    def unapply(n: ASTNode): Option[(String, Seq[ASTTree])] = Some((n.kind, n.sub.asInstanceOf[Seq[ASTTree]]))
  }

  object Lit {
    def unapply(n: ASTLiteral): Option[(String, Any)] = Some((n.kind, n.v))
  }

  def posToPos(ap: ASTPosition): Positioned = {
    new Positioned {
      override def hasPosition = ap.file.isDefined
      override def file = ap.file.asInstanceOf[Option[File]].get
      override def line = ap.line
      override def col  = ap.col
    }
  }


}
