package toolc
package ast

import grammarcomp.parsing.ParseTreeUtils
import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import grammarcomp.grammar.CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser2 extends Pipeline[Iterator[Token], Unit] {

  val toolGrammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ EXTENDS() ~ 'Identifier ~ 'ClassBody
      | CLASS() ~ 'Identifier ~ 'ClassBody,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ 'ElseOpt
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
      | 'Identifier ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | BANG() ~ 'Expression
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT))

  val precGrammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls ~ EOF() | 'MainObject ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE()
      | PROGRAM() ~ 'Identifier ~ LBRACE() ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | 'Statement,
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | 'ClassDeclaration,
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ EXTENDS() ~ 'Identifier ~ 'ClassBody
      | CLASS() ~ 'Identifier ~ 'ClassBody,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE()
      | LBRACE() ~ 'VarDecs ~ RBRACE()
      | LBRACE() ~ 'MethodDecs ~ RBRACE()
      | LBRACE() ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | 'VarDeclaration,
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | 'MethodDeclaration,
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ 'ParamsOpt ~ COLON() ~ 'Type ~ EQSIGN() ~ 'MethodBody ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'ParamsOpt ::= LPAREN() ~ RPAREN() | LPAREN() ~ 'Params ~ RPAREN(),
    'Params ::= 'Param ~ COMMA() ~ 'Params | 'Param,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'MethodBody ::= LBRACE() ~ 'VarDecs ~ 'Stmts
      | LBRACE() ~ 'VarDecs
      | LBRACE() ~ 'Stmts
      | LBRACE(),
    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
      | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ ELSE() ~ 'Statement
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
      | 'Identifier ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'Expression ::= 'Expr7,
    'Expr7 ::= 'Expr6 | 'Expr7 ~ OR() ~ 'Expr6,
    'Expr6 ::= 'Expr5 | 'Expr6 ~ AND() ~ 'Expr5,
    'Expr5 ::= 'Expr4 | 'Expr5 ~ LESSTHAN() ~ 'Expr4 | 'Expr5 ~ EQUALS() ~ 'Expr4,
    'Expr4 ::= 'Expr3 | 'Expr4 ~ PLUS() ~ 'Expr3 | 'Expr4 ~ MINUS() ~ 'Expr3,
    'Expr3 ::= 'Expr2 | 'Expr3 ~ TIMES() ~ 'Expr2 | 'Expr3 ~ DIV() ~ 'Expr2,
    'Expr2 ::= 'Expr1 | BANG() ~ 'Expr2,
    'Expr1 ::= 'Expr0
      | 'Expr0 ~ DOT() ~ LENGTH()
      | 'Expr0 ~ DOT() ~ 'Identifier ~ 'Args
      | 'Expr0 ~ LBRACKET() ~ 'Expression ~ RBRACKET(),
    'Expr0 ::= INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE()
      | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= LPAREN() ~ RPAREN() | LPAREN() ~ 'ExprList ~ RPAREN(),
    'ExprList ::= 'Expression | 'Expression ~ COMMA() ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT))

  val ll1grammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type ::= INT() ~ 'IntType | BOOLEAN() | STRING() | 'Identifier,
    'IntType ::= LBRACKET() ~ RBRACKET() | epsilon(),
    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Atom ~ 'Operation,
    'Operation ::= 'Op ~ 'Expression
      | epsilon(),
    'Atom ::= 'SimpleAtom ~ 'AtomTail
      | BANG() ~ 'Atom,
    'AtomTail ::= LBRACKET() ~ 'Expression ~ RBRACKET()
      | DOT() ~ 'Dotted
      | epsilon(),
    'SimpleAtom ::= INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ 'NewBody
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Dotted ::= LENGTH()
      | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN() ~ 'AtomTail,
    'NewBody ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Identifier ~ LPAREN() ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT))

  def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    if (!GrammarUtils.isLL1(ll1grammar))
      throw new Exception("The grammar is not LL(1)")
    val ptrees = ParseTreeUtils.parseWithTrees(ll1grammar, list)
    //println(ParseTreeUtils.parseTreetoString(ptrees(0)))
    if (ptrees.isEmpty) {
      println("Program Not Parsable!")
    } else {
      val ast = constructAST(ptrees.head)
      println("AST: " + Printer(ast))
      val refAST = Parser.run(ctx)(list.iterator)
      if(ast != refAST) {
        throw new Exception(s"Reference AST was different at the Node: ${(t1, t2)}")
        compareASTs(ast, refAST)        
      }
    }
  }
    
  def compareASTs(t1: Tree, t2: Tree): (Tree, Tree) = {
            
  }

  def constructAST(inptree: ParseTree[Token]): Program = {
    // Doing casting to avoid creating multiple methods
    def recTree(ptree: ParseTree[Token]): Tree = {
      ptree match {
        case Node(Rule(Nonterminal('Goal), _), List(mobj, classdefs, eof)) =>
          Program(recTree(mobj).asInstanceOf[MainObject],
            recTreeList(classdefs).asInstanceOf[List[ClassDecl]])
        case Node(Rule(Nonterminal('MainObject), _), List(_, objid, _, stmts, _)) =>
          MainObject(recId(objid), recTreeList(stmts).asInstanceOf[List[StatTree]])
        //::=	CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
        case Node(Rule(Nonterminal('ClassDeclaration), _), List(_, id, optextends,
          Node(Rule(Nonterminal('ClassBody), _), List(_, vardecls, methoddecls, _)))) =>
          ClassDecl(recId(id),
            recTreeOpt(optextends).asInstanceOf[Option[Identifier]],
            recTreeList(vardecls).asInstanceOf[List[VarDecl]],
            recTreeList(methoddecls).asInstanceOf[List[MethodDecl]])
        // 'VarDeclaration	::= VAR() ~ 'Param ~ SEMICOLON(),
        case Node(Rule(Nonterminal('VarDeclaration), _), List(_, param, _)) =>
          val Formal(tpe, id) = recParam(param)
          VarDecl(tpe, id)
        // 'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
        case Node(Rule(Nonterminal('MethodDeclaration), _), List(_, id, _, params, _, _, tpe, _, _, vardecs, stmts, _, expr, _, _)) =>
          MethodDecl(recId(id),
            (recTreeList(params).asInstanceOf[List[Formal]]),
            recType(tpe),
            recTreeList(vardecs).asInstanceOf[List[VarDecl]],
            recTreeList(stmts).asInstanceOf[List[StatTree]],
            recExpr(expr))
      }
    }

    def recParam(ptree: ParseTree[Token]): Formal = {
      ptree match {
        // 'Param ::= 'Identifier ~ COLON() ~ 'Type,
        case Node(Rule(Nonterminal('Param), _), List(id, _, tpe)) =>
          Formal(recType(tpe), recId(id))
      }
    }

    def recId(ptree: ParseTree[Token]): Identifier = {
      ptree match {
        case Node(Rule(Nonterminal('Identifier), _), List(Leaf(Terminal(ID(name))))) => Identifier(name)
      }
    }

    def recType(ptree: ParseTree[Token]): TypeTree = {
      ptree match {
        // 'Type	::=	INT() ~ 'IntType
        // 'IntType ::= LBRACKET() ~ RBRACKET() | epsilon(),
        case Node(Rule(Nonterminal('Type), _), List(_, Node(Rule(Nonterminal('IntType), _), suf))) =>
          if (suf.isEmpty) IntType()
          else IntArrayType()
        // BOOLEAN() | STRING() | 'Identifier, 
        case Node(Rule(Nonterminal('Type), _), List(Leaf(Terminal(l)))) =>
          l match {
            case BOOLEAN() => BooleanType()
            case STRING()  => StringType()
          }
        case Node(Rule(Nonterminal('Type), _), List(id)) => // this has to be identifier case
          ClassType(recId(id))
      }
    }

    def recStmt(ptree: ParseTree[Token]): StatTree = {
      // 'Statement ::=  IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      ptree match {
        case Node(Rule(Nonterminal('Statement), Terminal(IF()) :: _), List(_, _, expr, _, matchif, eopt)) =>
          If(recExpr(expr), recStmt(matchif), recTreeOpt(eopt).asInstanceOf[Option[StatTree]])
        //'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
        case Node(Rule(Nonterminal('Statement), Terminal(IF()) :: _), List(_, _, expr, _, thenif, _, eif)) =>
          If(recExpr(expr), recStmt(thenif), Some(recStmt(eif)))
        case Node(Rule(_, List(Nonterminal('SimpleStat))), List(simpstat)) =>
          recStmt(simpstat)
        /*'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
                  | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
                  | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
                  | 'Identifier ~ 'IdStat
                  | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),*/
        case Node(Rule(Nonterminal('SimpleStat), Terminal(LBRACE()) :: _), List(_, stmts, _)) =>
          Block(recTreeList(stmts).asInstanceOf[List[StatTree]])
        case Node(Rule(Nonterminal('SimpleStat), Terminal(WHILE()) :: _), List(_, _, expr, _, stmt)) =>
          While(recExpr(expr), recStmt(stmt))
        case Node(Rule(Nonterminal('SimpleStat), Terminal(PRINTLN()) :: _), List(_, _, expr, _, _)) =>
          Println(recExpr(expr))
        case Node(Rule(Nonterminal('SimpleStat), Terminal(DO()) :: _), List(_, _, expr, _, _)) =>
          DoExpr(recExpr(expr))
        case Node(Rule(Nonterminal('SimpleStat), rhs), List(id, idstat)) =>
          /*'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
              | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),*/
          idstat match {
            case Node(Rule(_, Terminal(EQSIGN()) :: _), List(_, expr, _)) =>
              Assign(recId(id), recExpr(expr))
            case Node(_, List(_, index, _, _, expr, _)) =>
              ArrayAssign(recId(id), recExpr(index), recExpr(expr))
          }
        case Node(Rule(Nonterminal('Statement), rhs), children) =>
          throw new Exception(s"rhs size: ${rhs.size} chidren nodes: ${children.size}")
      }
    }

    def getOp(ptree: ParseTree[Token]): Token = {
      ptree match {
        case Node(_, List(Leaf(Terminal(t)))) => t
      }
    }

    def recOp(ptree: ParseTree[Token]): (ExprTree, ExprTree) => ExprTree = {
      getOp(ptree) match {
        case AND()      => And.apply _
        case OR()       => Or.apply _
        case EQUALS()   => Equals.apply _
        case LESSTHAN() => LessThan.apply _
        case PLUS()     => Plus.apply _
        case MINUS()    => Minus.apply _
        case TIMES()    => Times.apply _
        case DIV()      => Div.apply _
      }
    }

    def precedence(iop1: Token, iop2: Token): Option[Boolean] = {
      def isArith(op: Token) = op match {
        case PLUS() | TIMES() | MINUS() | DIV() => true
        case _                                  => false
      }
      def greaterPrec(op1: Token, op2: Token) = (op1, op2) match {
        case (TIMES() | DIV(), PLUS() | MINUS())   => true
        case _ if isArith(op1) && !isArith(op2)    => true
        case (EQUALS() | LESSTHAN(), AND() | OR()) => true
        case _                                     => false
      }
      if (greaterPrec(iop1, iop2)) Some(true)
      else if (greaterPrec(iop2, iop1)) Some(false)
      else None
    }

    def addExprToLeft(leftop: ExprTree, e: ExprTree, op: (ExprTree, ExprTree) => ExprTree): ExprTree = {
      e match {
        case And(lhs, rhs) =>
          And(addExprToLeft(leftop, lhs, op), rhs)
        case Or(lhs, rhs) =>
          Or(addExprToLeft(leftop, lhs, op), rhs)
        case Plus(lhs, rhs) =>
          Plus(addExprToLeft(leftop, lhs, op), rhs)
        case Minus(lhs, rhs) =>
          Minus(addExprToLeft(leftop, lhs, op), rhs)
        case Times(lhs, rhs) =>
          Times(addExprToLeft(leftop, lhs, op), rhs)
        case Div(lhs, rhs) =>
          Div(addExprToLeft(leftop, lhs, op), rhs)
        case LessThan(lhs, rhs) =>
          LessThan(addExprToLeft(leftop, lhs, op), rhs)
        case Equals(lhs, rhs) =>
          Equals(addExprToLeft(leftop, lhs, op), rhs)
        case leaf =>
          // this is the left most leaf
          op(leftop, leaf)
      }
    }

    def recExpr(ptree: ParseTree[Token]): ExprTree = {
      ptree match {
        /*'Expression ::= 'Atom ~ 'Operation, 
         * 'Operation ::= 'Op ~ 'Expression | epsilon(), */
        case Node(Rule(Nonterminal('Expression), _), List(opd1, oper)) =>
          val opd1Expr = recExpr(opd1)
          oper match {
            case Node(_, List()) => opd1Expr
            case Node(_, List(op1, expr)) =>
              val op1Cons = recOp(op1)
              expr match {
                case Node(_, List(opd2, Node(_, List()))) =>
                  op1Cons(opd1Expr, recExpr(opd2)) // some thing like a  + b
                case Node(_, List(opd2, Node(_, List(op2, otherOpd)))) =>
                  val opd2Expr = recExpr(opd2)
                  // we need to check the precedences of op1 and op2
                  if (precedence(getOp(op1), getOp(op2)) == Some(false)) { // op1 has lesser precedence than op2
                    op1Cons(opd1Expr, opd2Expr) // some thing like a  + b * c                      
                  } else {
                    // here we have either a * b + c or a + b + c
                    addExprToLeft(opd1Expr, opd2Expr, op1Cons)
                  }
              }
          }
        /*Atom ::= 'SimpleAtom ~ 'AtomTail
            |  BANG() ~ 'Atom,*/
        case Node(Rule(Nonterminal('Atom), Terminal(BANG()) :: _), List(_, atom)) =>
          Not(recExpr(atom))
        case Node(Rule(Nonterminal('Atom), _), List(simpAtom, atomTail)) =>
          recAtomTail(recExpr(simpAtom), atomTail)
        /*''SimpleAtom ::= INTLITSENT | STRINGLITSENT
                | TRUE() | FALSE() | 'Identifier | THIS()
                | NEW() ~ 'NewBody
                | LPAREN() ~ 'Expression ~ RPAREN(),*/
        case Node(Rule(_, Terminal(NEW()) :: _), List(_, newbody)) => // new object creation
          newbody match {
            case Node(Rule(_, Terminal(INT()) :: _), List(_, _, sizeexpr, _)) =>
              NewIntArray(recExpr(sizeexpr))
            case Node(_, List(id, _, _)) => // class creation
              New(recId(id))
          }
        case Node(Rule(_, Terminal(LPAREN()) :: _), List(_, expr, _)) =>
          recExpr(expr)
        case Node(_, List(Leaf(Terminal(t)))) =>
          t match {
            case INTLIT(intval)  => IntLit(intval)
            case STRINGLIT(name) => StringLit(name)
            case TRUE()          => True()
            case FALSE()         => False()
            case THIS()          => This()
          }
        case Node(Rule(Nonterminal('SimpleAtom), _), List(id)) =>
          Variable(recId(id))
      }
    }

    /**
     * 'AtomTail ::= LBRACKET() ~ 'Expression ~ RBRACKET()
     * | DOT() ~ 'Dotted
     * | epsilon(),
     */
    def recAtomTail(startAtom: ExprTree, ptree: ParseTree[Token]): ExprTree = {
      ptree match {
        case Node(_, List()) => startAtom // epsilon rule
        case Node(Rule(_, Terminal(LBRACKET()) :: _), List(_, index, _)) =>
          ArrayRead(startAtom, recExpr(index))
        case Node(_, List(_, dotted)) =>
          /*'Dotted ::= LENGTH()
            | 'Identifier ~  LPAREN() ~ 'Args ~ RPAREN() ~ 'AtomTail,*/
          dotted match {
            case Node(_, List(Leaf(Terminal(LENGTH())))) => ArrayLength(startAtom)
            case Node(_, List(id, _, args, _, atomTail)) =>
              val mcall = MethodCall(startAtom, recId(id), recTreeList(args).asInstanceOf[List[ExprTree]])
              recAtomTail(mcall, atomTail)
          }
      }
    }

    /**
     * All closures
     */
    def recTreeList(ptree: ParseTree[Token]): List[Tree] = {
      ptree match {
        // All epsilon productions are handled here,
        case Node(_, List()) => List()
        // 'Stmts ::= 'Statement ~ 'Stmts
        case Node(Rule(Nonterminal('Stmts), _), List(stmt, stmts)) =>
          recStmt(stmt) +: recTreeList(stmts)
        //'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon()
        case Node(Rule(Nonterminal('ClassDecls), _), List(cdecl, decls)) =>
          recTree(cdecl) +: recTreeList(decls)
        //'vardecs
        case Node(Rule(Nonterminal('VarDecs), _), List(decl, vdecs)) =>
          recTree(decl) +: recTreeList(vdecs)
        //'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
        case Node(Rule(Nonterminal('MethodDecs), _), List(decl, vdecs)) =>
          recTree(decl) +: recTreeList(vdecs)
        //'Params ::= epsilon() | 'Param ~ 'ParamList,        
        case Node(Rule(Nonterminal('Params), _), List(param, paramlist)) =>
          recParam(param) +: recTreeList(paramlist)
        // 'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
        case Node(Rule(Nonterminal('ParamList), _), List(_, param, paramlist)) =>
          recParam(param) +: recTreeList(paramlist)
        /*'Args ::= epsilon() | 'Expression ~ 'ExprList,*/
        case Node(Rule(Nonterminal('Args), _), List(expr, exprlist)) =>
          recExpr(expr) +: recTreeList(exprlist)
        // 'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
        case Node(Rule(Nonterminal('ExprList), _), List(_, expr, exprlist)) =>
          recExpr(expr) +: recTreeList(exprlist)
      }
    }

    def recTreeOpt(ptree: ParseTree[Token]): Option[Tree] = {
      ptree match {
        case Node(_, List()) => None
        /*'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),*/
        case Node(Rule(Nonterminal('ElseOpt), _), List(_, stmt)) =>
          Some(recStmt(stmt))
        //'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
        case Node(Rule(Nonterminal('OptExtends), _), List(_, id)) =>
          Some(recId(id))
      }
    }
    recTree(inptree).asInstanceOf[Program]
  }

}