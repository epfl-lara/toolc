package toolc
package ast

import java.io.File
import utils._
import lexer._
import Trees._

object ASTDumper  {
  def apply(args: Array[String]): ASTNode = {
    val ctx = Main.processOptions(args)

    val pipeline =
      Lexer  andThen
      Parser

    dump(pipeline.run(ctx)(ctx.files.head))
  }

  def nameOf(t: Tree): String = t.getClass.getSimpleName

  def node(t: Tree, sub: ASTTree*) = {
    val p = new ASTPosition(t)
    new ASTNode(nameOf(t), p, sub.toList)
  }

  def lit(t: Tree, v: Any) = {
    val p = new ASTPosition(t)
    new ASTLiteral(nameOf(t), p, v)
  }

  def dump(ts: Seq[Tree]): ASTTree = new ASTList(ts.map(dump))
  def dump(ot: Option[Tree]): ASTTree = new ASTOption(ot.map(dump))

  def dump(t: Tree): ASTNode =  {
    t match {
      case Program(main, classes) =>
        node(t, dump(main), dump(classes))
      case MainObject(id, stats) =>
        node(t, dump(id), dump(stats))
      case ClassDecl(id, parent, vars, methods) =>
        node(t, dump(id), dump(parent), dump(vars), dump(methods))
      case VarDecl(tpe, id) =>
        node(t, dump(tpe), dump(id))
      case MethodDecl(retType, id, args, vars, stats, retExpr) =>
        node(t, dump(retType), dump(id), dump(args), dump(vars), dump(stats), dump(retExpr))
      case Formal(tpe, id) =>
        node(t, dump(tpe), dump(id))
      case IntArrayType() =>
        node(t)
      case IntType() =>
        node(t)
      case BooleanType() =>
        node(t)
      case StringType() =>
        node(t)
      case ClassType(id) =>
        lit(t, id.value)
      case Block(stats) =>
        node(t, dump(stats))
      case If(expr, thn, els) =>
        node(t, dump(expr), dump(thn), dump(els))
      case While(expr, stat) =>
        node(t, dump(expr), dump(stat))
      case Println(expr) =>
        node(t, dump(expr))
      case Assign(id, expr) =>
        node(t, dump(id), dump(expr))
      case ArrayAssign(id, index, expr) =>
        node(t, dump(id), dump(index), dump(expr))
      case DoExpr(expr) =>
        node(t, dump(expr))
      case And(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Or(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Plus(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Minus(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Times(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Div(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case LessThan(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case Equals(lhs, rhs) =>
        node(t, dump(lhs), dump(rhs))
      case ArrayRead(arr, index) =>
        node(t, dump(arr), dump(index))
      case ArrayLength(arr) =>
        node(t, dump(arr))
      case MethodCall(obj, meth, args) =>
        node(t, dump(obj), dump(meth), dump(args))
      case IntLit(value) =>
        lit(t, value)
      case StringLit(value) =>
        lit(t, value)
      case Identifier(value) =>
        lit(t, value)
      case This() =>
        node(t)
      case True() =>
        node(t)
      case False() =>
        node(t)
      case NewIntArray(size) =>
        node(t, dump(size))
      case New(tpe) =>
        node(t, dump(tpe))
      case Not(expr) =>
        node(t, dump(expr))
      case Variable(id) =>
        lit(t, id.value)
    }
  }
}

case class ASTPosition(file: Option[File], line: Int, col: Int) {
  def this(p: Positioned) = {
    this(if (p.hasPosition) Some(p.file) else None, p.line, p.col)
  }
}

abstract class ASTTree
class ASTList(val l: Seq[ASTNode]) extends ASTTree
class ASTOption(val o: Option[ASTNode]) extends ASTTree
class ASTNode(val kind: String, val p: ASTPosition, val sub: Seq[ASTTree]) extends ASTTree
class ASTLiteral(kind: String, p: ASTPosition, val v: Any) extends ASTNode(kind, p, Nil)


