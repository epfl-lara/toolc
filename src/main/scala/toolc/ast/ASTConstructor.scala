package toolc
package ast

import ast.Trees._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

object ASTConstructor {

  // Ulgy hack: doing casting to avoid creating multiple methods.
  // Is this a usecase of generalized ADTs ?
  def constructAST(inptree: NodeOrLeaf[Token]): Program = {
    def recTree(ptree: NodeOrLeaf[Token]): Tree = {
      ptree match {
        case Node('Goal ::= _, List(mobj, classdefs, eof)) =>
          Program(recTree(mobj).asInstanceOf[MainObject],
            recTreeList(classdefs).asInstanceOf[List[ClassDecl]])
        case Node('MainObject ::= _, List(_, objid, _, stmts, _)) =>
          MainObject(recId(objid), recTreeList(stmts).asInstanceOf[List[StatTree]])
        case Node('ClassDeclaration ::=  _, List(_, id, optextends,
        Node('ClassBody ::= _, List(_, vardecls, methoddecls, _)))) =>
          ClassDecl(recId(id),
            recTreeOpt(optextends).asInstanceOf[Option[Identifier]],
            recTreeList(vardecls).asInstanceOf[List[VarDecl]],
            recTreeList(methoddecls).asInstanceOf[List[MethodDecl]])
        case Node('VarDeclaration ::= _, List(_, param, _)) =>
          val Formal(tpe, id) = recParam(param)
          VarDecl(tpe, id)
        case Node('MethodDeclaration ::= _, List(_, id, _, params, _, _, tpe, _, _, vardecs, stmts, _, expr, _, _)) =>
          MethodDecl(recId(id),
            recTreeList(params).asInstanceOf[List[Formal]],
            recType(tpe),
            recTreeList(vardecs).asInstanceOf[List[VarDecl]],
            recTreeList(stmts).asInstanceOf[List[StatTree]],
            recExpr(expr))
      }
    }

    def recParam(ptree: NodeOrLeaf[Token]): Formal = {
      ptree match {
        case Node('Param ::= _, List(id, _, tpe)) =>
          Formal(recType(tpe), recId(id))
      }
    }

    def recId(ptree: NodeOrLeaf[Token]): Identifier = {
      ptree match {
        case Node('Identifier ::= _, List(Leaf(ID(name)))) =>
          Identifier(name)
      }
    }

    def recType(ptree: NodeOrLeaf[Token]): TypeTree = {
      ptree match {
        case Node('Type ::= _, List(_, Node('IntType ::= _, suf))) =>
          if (suf.isEmpty) IntType()
          else IntArrayType()
        case Node('Type ::= _, List(Leaf(l))) =>
          l match {
            case BOOLEAN() => BooleanType()
            case STRING()  => StringType()
          }
        case Node('Type ::=  _, List(id)) =>
          ClassType(recId(id))
      }
    }

    def recStmt(ptree: NodeOrLeaf[Token]): StatTree = {
      ptree match {
        case Node('Statement ::= IF() :: _, List(_, _, expr, _, matchif, eopt)) =>
          If(recExpr(expr), recStmt(matchif), recTreeOpt(eopt).asInstanceOf[Option[StatTree]])
        case Node('Statement ::= IF() :: _, List(_, _, expr, _, thenif, _, eif)) =>
          If(recExpr(expr), recStmt(thenif), Some(recStmt(eif)))
        case Node(_ ::= List('SimpleStat), List(simpstat)) =>
          recStmt(simpstat)
        case Node('SimpleStat ::= LBRACE() :: _, List(_, stmts, _)) =>
          Block(recTreeList(stmts).asInstanceOf[List[StatTree]])
        case Node('SimpleStat ::= WHILE() :: _, List(_, _, expr, _, stmt)) =>
          While(recExpr(expr), recStmt(stmt))
        case Node('SimpleStat ::= PRINTLN() :: _, List(_, _, expr, _, _)) =>
          Println(recExpr(expr))
        case Node('SimpleStat ::= DO() :: _, List(_, _, expr, _, _)) =>
          DoExpr(recExpr(expr))
        case Node('SimpleStat ::= rhs, List(id, idstat)) =>
          idstat match {
            case Node(_ ::= EQSIGN() :: _, List(_, expr, _)) =>
              Assign(recId(id), recExpr(expr))
            case Node(_, List(_, index, _, _, expr, _)) =>
              ArrayAssign(recId(id), recExpr(index), recExpr(expr))
          }
        /*case Node('Statement ::= rhs, children) =>
          throw new Exception(s"rhs size: ${rhs.size} chidren nodes: ${children.size}")*/
      }
    }

    def recOp(ptree: NodeOrLeaf[Token]): (ExprTree, ExprTree) => ExprTree = {
      ptree match {
        case Node(_, List(Leaf(t))) => t match {
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

    def recExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
      ptree match {
        case Node(sym ::= _, List(opd, suf)) if sym == 'Expression || sym == 'ArithExpr || sym == 'CompExpr || sym == 'Factor || sym == 'Disjunct =>
          recOpExpr(recExpr(opd), suf)
        case Node('Atom ::= BANG() :: _, List(_, atom)) =>
          Not(recExpr(atom))
        case Node('Atom ::= _, List(simpAtom, atomTail)) =>
          recAtomTail(recExpr(simpAtom), atomTail)
        case Node(_ ::= NEW() :: _, List(_, newbody)) => // new object creation
          newbody match {
            case Node(_ ::= INT() :: _, List(_, _, sizeexpr, _)) =>
              NewIntArray(recExpr(sizeexpr))
            case Node(_, List(id, _, _)) => // class creation
              New(recId(id))
          }
        case Node(_ ::= LPAREN():: _, List(_, expr, _)) =>
          recExpr(expr)
        case Node(_, List(Leaf(t))) =>
          t match {
            case INTLIT(intval)  => IntLit(intval)
            case STRINGLIT(name) => StringLit(name)
            case TRUE()          => True()
            case FALSE()         => False()
            case THIS()          => This()
          }
        case Node('SimpleAtom ::= _, List(id)) =>
          Variable(recId(id))
      }
    }

    /**
      *  'LogicalExp ::= 'LogicalOp ~ 'Expression | epsilon(),
      * 'RelExp ::= 'RelOp ~ 'CompExpr | epsilon(),
      * 'SumExp ::= 'SumOp ~ 'ArithExpr,
      * 'MultExp ::= 'MultOp ~ 'Factor | epsilon(),
      * */
    def recOpExpr(leftopd: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
      ptree match {
        case Node(_, List()) => //epsilon rule of the nonterminals
          leftopd
        case Node(sym ::= _, List(op, rightNode)) if sym == 'OrExp || sym == 'RelExp || sym == 'SumExp || sym == 'MultExp || sym == 'AndExp =>
          rightNode match {
            case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
              val nextAtom = recExpr(nextOpd)
              suf match {
                case Node(_, List()) =>
                  recOp(op)(leftopd, nextAtom) // some thing like a  + b
                case Node(_, List(op2, nextNode)) =>
                  recOpExpr(recOp(op)(leftopd, nextAtom), suf) // captures left associativity
              }
          }
      }
    }

    /**
      * 'AtomTail ::= LBRACKET() ~ 'Expression ~ RBRACKET()
      * | DOT() ~ 'Dotted
      * | epsilon(),
      */
    def recAtomTail(startAtom: ExprTree, ptree: NodeOrLeaf[Token]): ExprTree = {
      ptree match {
        case Node(_, List()) => startAtom // epsilon rule
        case Node(_ ::= LBRACKET() :: _, List(_, index, _)) =>
          ArrayRead(startAtom, recExpr(index))
        case Node(_, List(_, dotted)) =>
          dotted match {
            case Node(_, List(Leaf(LENGTH()))) => ArrayLength(startAtom)
            case Node(_, List(id, _, args, _, atomTail)) =>
              val mcall = MethodCall(startAtom, recId(id), recTreeList(args).asInstanceOf[List[ExprTree]])
              recAtomTail(mcall, atomTail)
          }
      }
    }

    /**
      * All kleene  closures
      */
    def recTreeList(ptree: NodeOrLeaf[Token]): List[Tree] = {
      ptree match {
        // All epsilon productions are handled here,
        case Node(_, List()) => List()
        case Node(sym ::= _, List(decl, decls))
          if sym == 'ClassDecls || sym == 'VarDecs || sym == 'MethodDecs =>
          recTree(decl) +: recTreeList(decls)
        case Node('Stmts ::=  _, List(stmt, stmts)) =>
          recStmt(stmt) :: recTreeList(stmts)
        case Node('Params ::= _, List(param, paramlist)) =>
          recParam(param) +: recTreeList(paramlist)
        case Node('ParamList ::=  _, List(_, param, paramlist)) =>
          recParam(param) +: recTreeList(paramlist)
        case Node('Args ::= _, List(expr, exprlist)) =>
          recExpr(expr) +: recTreeList(exprlist)
        case Node('ExprList ::=  _, List(_, expr, exprlist)) =>
          recExpr(expr) +: recTreeList(exprlist)
      }
    }

    def recTreeOpt(ptree: NodeOrLeaf[Token]): Option[Tree] = {
      ptree match {
        case Node(_, List()) => None
        case Node('ElseOpt ::= _, List(_, stmt)) =>
          Some(recStmt(stmt))
        case Node('OptExtends ::=  _, List(_, id)) =>
          Some(recId(id))
      }
    }
    recTree(inptree).asInstanceOf[Program]
  }
}
