package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= _, List(_, Node('IntType ::= _, suf))) =>
        if (suf.isEmpty) IntType()
        else IntArrayType()
      case Node('Type ::= List(BOOLEAN()), _) =>
        BooleanType()
      case Node('Type ::= List(STRING()), _) =>
        StringType()
      case Node('Type ::= List('Identifier), List(id)) =>
        ClassType(constructId(id))
    }
  }


  override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node(sym ::= _, List(opd, suf)) if sym == 'Expression || sym == 'ArithExpr || sym == 'CompExpr || sym == 'Factor || sym == 'Disjunct =>
        constructOpExpr(constructExpr(opd), suf)
      case Node('Atom ::= BANG() :: _, List(_, atom)) =>
        Not(constructExpr(atom))
      case Node('Atom ::= _, List(simpAtom, atomTail)) =>
        constructAtomTail(constructExpr(simpAtom), atomTail)
      case Node(_ ::= NEW() :: _, List(_, newbody)) => // new object creation
        newbody match {
          case Node(_ ::= INT() :: _, List(_, _, sizeexpr, _)) =>
            NewIntArray(constructExpr(sizeexpr))
          case Node(_, List(id, _, _)) => // class creation
            New(constructId(id))
        }
      case Node(_ ::= LPAREN():: _, List(_, expr, _)) =>
        constructExpr(expr)
      case Node(_, List(Leaf(t))) =>
        (t: @unchecked) match {
          case INTLIT(intval)  => IntLit(intval)
          case STRINGLIT(name) => StringLit(name)
          case TRUE()          => True()
          case FALSE()         => False()
          case THIS()          => This()
        }
      case Node('SimpleAtom ::= _, List(id)) =>
        Variable(constructId(id))
    }
  }

  /**
    *  'LogicalExp ::= 'LogicalOp ~ 'Expression | epsilon(),
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
            suf match {
              case Node(_, List()) =>
                constructOp(op)(leftopd, nextAtom) // some thing like a  + b
              case Node(_, List(op2, nextNode)) =>
                constructOpExpr(constructOp(op)(leftopd, nextAtom), suf) // captures left associativity
            }
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
      case Node(_ ::= LBRACKET() :: _, List(_, index, _)) =>
        ArrayRead(startAtom, constructExpr(index))
      case Node(_, List(_, dotted)) =>
        dotted match {
          case Node(_, List(Leaf(LENGTH()))) => ArrayLength(startAtom)
          case Node(_, List(id, _, args, _, atomTail)) =>
            val mcall = MethodCall(startAtom, constructId(id), constructList(args, constructExpr, hasComma = true).asInstanceOf[List[ExprTree]])
            constructAtomTail(mcall, atomTail)
        }
    }
  }

}
