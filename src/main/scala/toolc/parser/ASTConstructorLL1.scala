package toolc
package parser

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(Leaf(nt), Node('IntType ::= _, suf))) =>
        (
          if (suf.isEmpty) IntType()
          else IntArrayType()
        ).setPos(nt)
      case Node('Type ::= _, List(Leaf(bt@BOOLEAN()))) =>
        BooleanType().setPos(bt)
      case Node('Type ::= _, List(Leaf(st@STRING()))) =>
        StringType().setPos(st)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }


  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node(sym ::= _, List(opd, suf)) if sym == 'Expression || sym == 'ArithExpr || sym == 'CompExpr || sym == 'Factor || sym == 'Disjunct =>
        constructOpExpr(constructExpr(opd), suf)
      case Node('Atom ::= BANG() :: _, List(Leaf(bt), atom)) =>
        Not(constructExpr(atom)).setPos(bt)
      case Node('Atom ::= _, List(simpAtom, atomTail)) =>
        val psa = constructExpr(simpAtom)
        constructAtomTail(psa, atomTail).setPos(psa)
      case Node(_ ::= NEW() :: _, List(Leaf(nt), newbody)) => // new object creation
        (newbody match {
          case Node(_ ::= INT() :: _, List(_, _, sizeexpr, _)) =>
            NewIntArray(constructExpr(sizeexpr))
          case Node(_, List(id, _, _)) => // class creation
            New(constructId(id))
        }).setPos(nt)
      case Node(_ ::= LPAREN():: _, List(Leaf(lp), expr, _)) =>
        constructExpr(expr).setPos(lp)
      case Node(_, List(Leaf(t))) =>
        ((t: @unchecked) match {
          case INTLIT(intval)  => IntLit(intval)
          case STRINGLIT(name) => StringLit(name)
          case TRUE()          => True()
          case FALSE()         => False()
          case THIS()          => This()
        }).setPos(t)
      case Node('SimpleAtom ::= _, List(id)) =>
        val pid = constructId(id)
        Variable(pid).setPos(pid)
    }
  }

  /**
    * 'LogicalExp ::= 'LogicalOp ~ 'Expression | epsilon(),
    * 'RelExp ::= 'RelOp ~ 'CompExpr | epsilon(),
    * 'SumExp ::= 'SumOp ~ 'ArithExpr,
    * 'MultExp ::= 'MultOp ~ 'Factor | epsilon(),
    * */
  def constructOpExpr(leftopd: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode)) if sym == 'OrExp || sym == 'RelExp || sym == 'SumExp || sym == 'MultExp || sym == 'AndExp =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

  /**
    * 'AtomTail ::= LBRACKET() ~ 'Expression ~ RBRACKET()
    * | DOT() ~ 'Dotted
    * | epsilon(),
    */
  def constructAtomTail(startAtom: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node(_, List()) => startAtom // epsilon rule
      case Node(_ ::= LBRACKET() :: _, List(_, index, _, atomTail)) =>
        constructAtomTail(ArrayRead(startAtom, constructExpr(index)), atomTail).setPos(startAtom)
      case Node(_, List(_, dotted)) =>
        dotted match {
          case Node(_, List(Leaf(LENGTH()), atomTail)) =>
            constructAtomTail(ArrayLength(startAtom), atomTail).setPos(startAtom)
          case Node(_, List(id, _, args, _, atomTail)) =>
            val mcall = MethodCall(startAtom, constructId(id), constructList(args, constructExpr, hasComma = true)).setPos(startAtom)
            constructAtomTail(mcall, atomTail).setPos(mcall)
        }
    }
  }

}
