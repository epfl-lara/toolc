package toolc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  // Unique symbols
  case class Identifier(value: String) extends Tree with Symbolic[Symbol] with Typed {
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol =>
        TClass(cs)

      case ms: MethodSymbol =>
        sys.error("Requesting type of a method identifier.")

      case vs: VariableSymbol =>
        vs.getType
    }
    override def setType(tpe: Type) = this
    override def toString = value
  }


  // Definitions
  sealed trait DefTree extends Tree
  case class Program(main: MainObject, classes: List[ClassDecl])
    extends DefTree
  case class MainObject(id: Identifier, stats: List[StatTree])
    extends DefTree with Symbolic[ClassSymbol]
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
    extends DefTree with Symbolic[ClassSymbol]
  case class VarDecl(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]
  case class MethodDecl(retType: TypeTree,
                        id: Identifier,
                        args: List[Formal],
                        vars: List[VarDecl],
                        stats: List[StatTree],
                        retExpr: ExprTree)
    extends DefTree with Symbolic[MethodSymbol]
  sealed case class Formal(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]

  // Types
  sealed trait TypeTree extends Tree with Typed
  case class IntArrayType() extends TypeTree {
    override def getType = TIntArray
  }
  case class IntType() extends TypeTree {
    override def getType = TInt
  }
  case class BooleanType() extends TypeTree {
    override def getType = TBoolean
  }
  case class StringType() extends TypeTree {
    override def getType = TString
  }
  case class ClassType(id: Identifier) extends TypeTree {
    override def getType = id.getType
  }

  // Statements
  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree
  case class DoExpr(e: ExprTree) extends StatTree

  // Expressions
  sealed trait ExprTree extends Tree with Typed
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree
  case class ArrayLength(arr: ExprTree) extends ExprTree
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree
  case class IntLit(value: Int) extends ExprTree
  case class StringLit(value: String) extends ExprTree
  case class Variable(id: Identifier) extends ExprTree
  case class True() extends ExprTree
  case class False() extends ExprTree
  case class This() extends ExprTree with Symbolic[ClassSymbol]
  case class NewIntArray(size: ExprTree) extends ExprTree
  case class New(tpe: Identifier) extends ExprTree
  case class Not(expr: ExprTree) extends ExprTree
}
