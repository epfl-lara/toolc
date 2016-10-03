package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructor {

  def constructProgram(ptree: NodeOrLeaf[Token]): Program = {
    ptree match {
      case Node('Program ::= _, List(mainObj, classDefs, eof)) =>
        Program(
          constructMain(mainObj),
          constructList(classDefs, constructClass)
        )
    }
  }
  def constructMain(ptree: NodeOrLeaf[Token]): MainObject = {
    ptree match {
      case Node('MainObject ::= _, List(_, objid, _, stmts, _)) =>
        MainObject(constructId(objid), constructList(stmts, constructStatement))
    }
  }

  def constructClass(ptree: NodeOrLeaf[Token]): ClassDecl = {
    ptree match {
      case Node(
        'ClassDeclaration ::=  _,
        List(_, id, optextends, Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))
      ) =>
        ClassDecl(
          constructId(id),
          constructOption(optextends, constructId),
          constructList(vardecls, constructVarDecl),
          constructList(methoddecls, constructMethodDecl)
        )
    }
  }

  def constructVarDecl(ptree: NodeOrLeaf[Token]): VarDecl = ptree match {
    case Node('VarDeclaration ::= _, List(_, param, _)) =>
      val Formal(tpe, id) = constructParam(param)
      VarDecl(tpe, id)
  }

  def constructMethodDecl(ptree: NodeOrLeaf[Token]): MethodDecl = ptree match {
    case Node('MethodDeclaration ::= _, List(_, id, _, params, _, _, tpe, _, _, vardecs, stmts, _, expr, _, _)) =>
      MethodDecl(
        constructId(id),
        constructList(params, constructParam, hasComma = true),
        constructType(tpe),
        constructList(vardecs, constructVarDecl),
        constructList(stmts, constructStatement),
        constructExpr(expr)
      )
  }

  def constructParam(ptree: NodeOrLeaf[Token]): Formal = {
    ptree match {
      case Node('Param ::= _, List(id, _, tpe)) =>
        Formal(constructType(tpe), constructId(id))
    }
  }

  def constructId(ptree: NodeOrLeaf[Token]): Identifier = {
    ptree match {
      case Node('Identifier ::= _, List(Leaf(ID(name)))) =>
        Identifier(name)
    }
  }

  def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= List(INT()), _) =>
        IntType()
      case Node('Type ::= List(INT(), LBRACKET(), RBRACKET()), _) =>
        IntArrayType()
      case Node('Type ::= List(BOOLEAN()), _) =>
        BooleanType()
      case Node('Type ::= List(STRING()), _) =>
        StringType()
      case Node('Type ::= List(IDSENT), List(id)) =>
        ClassType(constructId(id))
    }
  }


  def constructStatement(ptree: NodeOrLeaf[Token]): StatTree = {
    ptree match {
      case Node('Statement ::= IF() :: _, List(_, _, expr, _, matchif, eopt)) =>
        If(constructExpr(expr), constructStatement(matchif), constructOption(eopt, constructStatement))
      case Node('Statement ::= IF() :: _, List(_, _, expr, _, thenif, _, eif)) =>
        If(constructExpr(expr), constructStatement(thenif), Some(constructStatement(eif)))
      case Node(_ ::= List('SimpleStat), List(simpstat)) =>
        constructStatement(simpstat)
      case Node('SimpleStat ::= LBRACE() :: _, List(_, stmts, _)) =>
        Block(constructList(stmts, constructStatement))
      case Node('SimpleStat ::= WHILE() :: _, List(_, _, expr, _, stmt)) =>
        While(constructExpr(expr), constructStatement(stmt))
      case Node('SimpleStat ::= PRINTLN() :: _, List(_, _, expr, _, _)) =>
        Println(constructExpr(expr))
      case Node('SimpleStat ::= DO() :: _, List(_, _, expr, _, _)) =>
        DoExpr(constructExpr(expr))
      case Node('SimpleStat ::= rhs, List(id, idstat)) =>
        idstat match {
          case Node(_ ::= EQSIGN() :: _, List(_, expr, _)) =>
            Assign(constructId(id), constructExpr(expr))
          case Node(_, List(_, index, _, _, expr, _)) =>
            ArrayAssign(constructId(id), constructExpr(index), constructExpr(expr))
        }
    }
  }

  def constructOp(ptree: NodeOrLeaf[Token]): (ExprTree, ExprTree) => ExprTree = {
    ptree match {
      case Node(_, List(Leaf(t))) => (t: @unchecked) match {
        case AND()      => And
        case OR()       => Or
        case EQUALS()   => Equals
        case LESSTHAN() => LessThan
        case PLUS()     => Plus
        case MINUS()    => Minus
        case TIMES()    => Times
        case DIV()      => Div
      }
    }
  }

  def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Expression ::= List('Expression, 'Op, 'Expression), List(e1, op, e2)) =>
        constructOp(op)(constructExpr(e1), constructExpr(e2))
      case Node('Expression ::= List('Expression, LBRACKET(), 'Expression, RBRACKET()), List(e1, _, e2, _)) =>
        ArrayRead(constructExpr(e1), constructExpr(e2))
      case Node('Expression ::= List('Expression, DOT(), LENGTH()), List(e, _, _)) =>
        ArrayLength(constructExpr(e))
      case Node('Expression ::= List('Expression, DOT(), 'Identifier, LPAREN(), 'Args, RPAREN()), List(e, _, id, _, as, _)) =>
        MethodCall(constructExpr(e), constructId(id), constructList(as, constructExpr, hasComma = true))
      case Node('Expression ::= List(INTLITSENT), List(Leaf(INTLIT(i)))) =>
        IntLit(i)
      case Node('Expression ::= List(STRINGLITSENT), List(Leaf(STRINGLIT(s)))) =>
        StringLit(s)
      case Node('Expression ::= List(TRUE()), _) =>
        True()
      case Node('Expression ::= List(FALSE()), _) =>
        False()
      case Node('Expression ::= List('Identifier), List(id)) =>
        Variable(constructId(id))
      case Node('Expression ::= List(THIS()), _) =>
        This()
      case Node('Expression ::= List(NEW(), INT(), LBRACKET(), 'Expression, RBRACKET()), List(_, _, _, e, _)) =>
        NewIntArray(constructExpr(e))
      case Node('Expression ::= List(NEW(), 'Identifier, LPAREN(), RPAREN()), List(_, id, _, _)) =>
        New(constructId(id))
      case Node('Expression ::= List(BANG(), 'Expression), List(_, e)) =>
        Not(constructExpr(e))
      case Node('Expression ::= List(LPAREN(), 'Expression, RPAREN()), List(_, e, _)) =>
        constructExpr(e)
    }
  }

  def constructList[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List()) => List()
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList(ts, constructor, hasComma)
      case Node(_, List(Leaf(COMMA()), t, ts)) if hasComma =>
        constructor(t) :: constructList(ts, constructor, hasComma)
    }
  }

  def constructOption[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A): Option[A] = {
    ptree match {
      case Node(_, List()) => None
      case Node(_, List(_, t)) =>
        Some(constructor(t))
    }
  }

}
