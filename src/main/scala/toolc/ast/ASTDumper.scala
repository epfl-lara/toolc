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

  def nameOf(t: Tree): String = t match {
    case Program(main: MainObject, classes: List[ClassDecl]) => "Program"
    case MainObject(id: Identifier, stats: List[StatTree]) => "MainObject"
    case ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) => "ClassDecl"
    case VarDecl(tpe: TypeTree, id: Identifier) => "VarDecl"
    case MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) => "MethodDecl"
    case Formal(tpe: TypeTree, id: Identifier) => "Formal"
    case IntArrayType() => "IntArrayType"
    case IntType() => "IntType"
    case BooleanType() => "BooleanType"
    case StringType() => "StringType"
    case Block(stats: List[StatTree]) => "Block"
    case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) => "If"
    case While(expr: ExprTree, stat: StatTree) => "While"
    case Println(expr: ExprTree) => "Println"
    case Assign(id: Identifier, expr: ExprTree) => "Assign"
    case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => "ArrayAssign"
    case And(lhs: ExprTree, rhs: ExprTree) => "And"
    case Or(lhs: ExprTree, rhs: ExprTree) => "Or"
    case Plus(lhs: ExprTree, rhs: ExprTree) => "Plus"
    case Minus(lhs: ExprTree, rhs: ExprTree) => "Minus"
    case Times(lhs: ExprTree, rhs: ExprTree) => "Times"
    case Div(lhs: ExprTree, rhs: ExprTree) => "Div"
    case LessThan(lhs: ExprTree, rhs: ExprTree) => "LessThan"
    case Equals(lhs: ExprTree, rhs: ExprTree) => "Equals"
    case ArrayRead(arr: ExprTree, index: ExprTree) => "ArrayRead"
    case ArrayLength(arr: ExprTree) => "ArrayLength"
    case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => "MethodCall"
    case IntLit(value: Int) => "IntLit"
    case StringLit(value: String) => "StringLit"
    case Identifier(value: String) => "Identifier"
    case This() => "This"
    case True() => "True"
    case False() => "False"
    case NewIntArray(size: ExprTree) => "NewIntArray"
    case New(tpe: Identifier) => "New"
    case Not(expr: ExprTree) => "Not"
  }

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
      case Program(main: MainObject, classes: List[ClassDecl]) =>
        node(t, dump(main), dump(classes))
      case MainObject(id: Identifier, stats: List[StatTree]) =>
        node(t, dump(id), dump(stats))
      case ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) =>
        node(t, dump(id), dump(parent), dump(vars), dump(methods))
      case VarDecl(tpe: TypeTree, id: Identifier) =>
        node(t, dump(tpe), dump(id))
      case MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree) =>
        node(t, dump(retType), dump(id), dump(args), dump(vars), dump(stats), dump(retExpr))
      case Formal(tpe: TypeTree, id: Identifier) =>
        node(t, dump(tpe), dump(id))
      case IntArrayType() =>
        node(t)
      case IntType() =>
        node(t)
      case BooleanType() =>
        node(t)
      case StringType() =>
        node(t)
      case Block(stats: List[StatTree]) =>
        node(t, dump(stats))
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        node(t, dump(expr), dump(thn), dump(els))
      case While(expr: ExprTree, stat: StatTree) =>
        node(t, dump(expr), dump(stat))
      case Println(expr: ExprTree) =>
        node(t, dump(expr))
      case Assign(id: Identifier, expr: ExprTree) =>
        node(t, dump(id), dump(expr))
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        node(t, dump(id), dump(index), dump(expr))
      case And(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Or(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Plus(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Minus(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Times(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Div(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case LessThan(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case Equals(lhs: ExprTree, rhs: ExprTree) =>
        node(t, dump(lhs), dump(rhs))
      case ArrayRead(arr: ExprTree, index: ExprTree) =>
        node(t, dump(arr), dump(index))
      case ArrayLength(arr: ExprTree) =>
        node(t, dump(arr))
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        node(t, dump(obj), dump(meth), dump(args))
      case IntLit(value: Int) =>
        lit(t, value)
      case StringLit(value: String) =>
        lit(t, value)
      case Identifier(value: String) =>
        lit(t, value)
      case This() =>
        node(t)
      case True() =>
        node(t)
      case False() =>
        node(t)
      case NewIntArray(size: ExprTree) =>
        node(t, dump(size))
      case New(tpe: Identifier) =>
        node(t, dump(tpe))
      case Not(expr: ExprTree) =>
        node(t, dump(expr))
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


