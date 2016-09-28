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
import java.lang.reflect._

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
    'Expression ::=  'Disjunct ~ 'OrExp,
    'OrExp ::= 'Or ~ 'Expression | epsilon(),
    'Disjunct ::= 'Expr1 ~ 'AndExp,
    'AndExp ::= 'And ~ 'Disjunct | epsilon(),
    'Expr1 ::= 'Expr2 ~ 'RelExp,
    'RelExp ::= 'RelOp ~ 'Expr1 | epsilon(),
    'Expr2 ::= 'Factor ~ 'SumExp,
    'SumExp ::= 'SumOp ~ 'Expr2 | epsilon(),
    'Factor ::= 'Atom ~ 'MultExp,
    'MultExp ::= 'MultOp ~ 'Factor | epsilon(),
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
    'Or ::= OR(),
    'And ::= AND(), 
    'RelOp ::= EQUALS() | LESSTHAN(), 
    'SumOp ::= PLUS() | MINUS(),
    'MultOp ::= TIMES() | DIV(),
    'Identifier ::= IDSENT))

  def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    if (!GrammarUtils.isLL1(ll1grammar))
      throw new Exception("The grammar is not LL(1)")    
    val startTime = System.currentTimeMillis()
    val ptrees = ParseTreeUtils.parseWithTrees(ll1grammar, list)    
    //val ptreesBU = parseBottomUp(list.map(CFGrammar.Terminal[Token] _))
    val nextTime = System.currentTimeMillis()
    println(s"Completed parsing in ${(nextTime - startTime)/1000.0} s")    
    //println("LL1 trees: "+ptrees(0))
    //println("BU trees: "+ptreesBU(0))
    if (ptrees.isEmpty) {
      println("Program Not Parsable!")
      /*if(bupres)
        throw new Exception("Bottom up parser returned a different result")*/
    } else {      
      val ast = constructAST(ptrees.head)
      println("AST: " + Printer(ast))
      val refAST = Parser.run(ctx)(list.iterator)
      if (ast != refAST) {
        println("ASTs are not equal!")
        //throw new Exception(s"Reference AST was different at the Node: ${(t1, t2)}")
        compareASTs(ast, refAST)
      }
    }
  }
  
  /* Used only for debugging CYK parser
   * def parseBottomUp(s: List[Terminal[Token]])(implicit opctx: GlobalContext) = {
    val cykParser = new CYKParser(ll1grammar.twonfGrammar)
    //val termclassWord = s.map { t => new grammarcomp.parsing.ParseTreeUtils.TerminalWrapper(t) }.toArray
    cykParser.parseWithTrees(s).map(t => ParseTreeDSL.mapTree(t))
  }*/

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
            (recTreeList(params).asInstanceOf[List[Formal]]),
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
    }
        
    def recExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
      ptree match {          
        case Node(sym ::= _, List(opd, suf)) if sym == 'Expression || sym == 'Expr2 || sym == 'Expr1 || sym == 'Factor || sym == 'Disjunct =>          
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
     * 'RelExp ::= 'RelOp ~ 'Expr1 | epsilon(),
     * 'SumExp ::= 'SumOp ~ 'Expr2,
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
  
  /**
   * A helper method that uses reflection to compare AST trees.
   * Used in testing.
   */
  def compareASTs(t1: Tree, t2: Tree): Unit = {
    if (t1.getClass() == t2.getClass()) {
      val declClass = t1.getClass()      
      val flds = declClass.getDeclaredFields()
      flds.foreach(_.setAccessible(true))
      if (flds.isEmpty)
        println("Didnt find any field with public modifier:" + declClass.getDeclaredFields().map(_.getName()))
      flds.foreach { field =>
        val f1 = field.get(t1)
        val f2 = field.get(t2)
        if (f1.isInstanceOf[Tree]) {
          compareASTs(f1.asInstanceOf[Tree], f2.asInstanceOf[Tree])
        } else if (f1.isInstanceOf[List[Tree]]) {
          (f1.asInstanceOf[List[Tree]] zip f2.asInstanceOf[List[Tree]]).foreach {
            case (x, y) => compareASTs(x, y)
          }
        }
        /*if (f1 != f2)
          throw new Exception(s"Reference AST was different at the children: ${(f1, f2)}")*/
      }
    } else
      throw new Exception(s"AST: $t1 RefAST: $t2")
  }

}