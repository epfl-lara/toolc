package toolc
package ast

import utils._
import ast.Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, and one lookahead token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while(currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if(currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
      readToken
      val mainObject = parseMainObject
      var classes: List[ClassDecl] = Nil
      while(currentToken.kind == CLASS) {
        classes = classes ::: (parseClassDecl :: Nil)
      }
      eat(EOF)
      Program(mainObject, classes).setPos(mainObject)
    }

    def parseMainObject: MainObject = {
      val pos = currentToken
      eat(OBJECT)
      val id = parseIdentifier
      eat(LBRACE); eat(DEF); eat(MAIN); eat(LPAREN); eat(RPAREN); eat(COLON); eat(UNIT); eat(EQSIGN); eat(LBRACE);
      val stats = parseStatements
      eat(RBRACE); eat(RBRACE)
      MainObject(id, stats).setPos(pos)
    }

    def parseClassDecl: ClassDecl = {
      val pos = currentToken
      eat(CLASS)
      val id = parseIdentifier
      val parent = if(currentToken.kind == EXTENDS) {
        readToken
        Some(parseIdentifier)
      } else {
        None
      }
      eat(LBRACE)
      val varDecls = parseVarDecls
      var methDecls: List[MethodDecl] = Nil
      while(currentToken.kind == DEF) {
        methDecls = methDecls ::: (parseMethodDecl :: Nil)
      }
      eat(RBRACE)
      ClassDecl(id, parent, varDecls, methDecls).setPos(pos)
    }

    def parseMethodDecl: MethodDecl = {
      val pos = currentToken
      eat(DEF)
      val id = parseIdentifier
      eat(LPAREN)
      val formals = parseFormals
      eat(RPAREN)
      eat(COLON)
      val retTpe = parseType
      eat(EQSIGN)
      eat(LBRACE)
      val varDecls = parseVarDecls
      val stats = parseStatements
      eat(RETURN)
      val retExpr = parseExpr
      eat(SEMICOLON)
      eat(RBRACE)
      MethodDecl(retTpe, id, formals, varDecls, stats, retExpr).setPos(pos)
    }

    def parseFormals: List[Formal] = {
      if(currentToken.kind != IDKIND)
        Nil
      else {  
        val id = parseIdentifier
        eat(COLON)
        val tpe = parseType

        var lst: List[Formal] = Formal(tpe, id) :: Nil

        while(currentToken.kind == COMMA) {
          readToken
          val id2 = parseIdentifier
          eat(COLON)
          val tpe2 = parseType
          lst = Formal(tpe2, id2) :: lst
        }
        lst.reverse
      }
    }

    def parseVarDecls: List[VarDecl] = {
      var lst: List[VarDecl] = Nil
      while(currentToken.kind == VAR) {
        lst = parseVarDecl :: lst
      }
      lst.reverse
    }

    def parseVarDecl: VarDecl = {
      eat(VAR)
      val id = parseIdentifier
      eat(COLON)
      val tpe = parseType
      eat(SEMICOLON)
      VarDecl(tpe, id).setPos(id)
    }

    def parseType: TypeTree = currentToken.kind match {
      case BOOLEAN => { val ret = BooleanType().setPos(currentToken); readToken; ret }
      case INT => {
        val pos = currentToken
        readToken
        if(currentToken.kind == LBRACKET) {
          readToken
          eat(RBRACKET)
          IntArrayType().setPos(pos)
        } else {
          IntType().setPos(pos)
        }
      }
      case STRING => { val ret = StringType().setPos(currentToken); readToken; ret }
      case IDKIND => parseIdentifier
      case _ => expected(BOOLEAN,INT,STRING,IDKIND)
    }
    
    def parseStatement: StatTree = currentToken.kind match {
      case LBRACE => {
        val pos = currentToken
        readToken
        val ret = Block(parseStatements).setPos(pos)
        eat(RBRACE)
        ret
      }
      case IF => {
        val pos = currentToken
        readToken
        eat(LPAREN)
        val cond = parseExpr
        eat(RPAREN)
        val thn = parseStatement
        val elz = if(currentToken.kind == ELSE) {
          readToken
          Some(parseStatement)
        } else {
          None
        }
        If(cond, thn, elz).setPos(pos)
      }
      case WHILE => {
        val pos = currentToken
        readToken
        eat(LPAREN)
        val cond = parseExpr
        eat(RPAREN)
        While(cond, parseStatement).setPos(pos)
      }
      case PRINTLN => {
        val pos = currentToken
        readToken
        eat(LPAREN)
        val expr = parseExpr
        eat(RPAREN)
        eat(SEMICOLON)
        Println(expr).setPos(pos)
      }
      case IDKIND => {
        val pos = currentToken
        val id = parseIdentifier
        if(currentToken.kind == EQSIGN) {
          readToken
          val expr = parseExpr
          eat(SEMICOLON)
          Assign(id, expr).setPos(pos)
        } else if(currentToken.kind == LBRACKET) {
          readToken
          val index = parseExpr
          eat(RBRACKET)
          eat(EQSIGN)
          val expr = parseExpr
          eat(SEMICOLON)
          ArrayAssign(id, index, expr).setPos(pos)
        } else {
          expected(EQSIGN,LBRACKET)
        }
      }
      case _ => expected(LBRACE,IF,WHILE,PRINTLN,IDKIND)
    }
    
    def parseStatements: List[StatTree] = {
      var lst: List[StatTree] = Nil
      while(statFirst) {
        lst = lst ::: (parseStatement :: Nil)
      }
      lst
    }
    
    // all expressions
    def parseExpr: ExprTree = parseExpr7
    
    // expr7 ::= expr6 ('||' expr6)*
    def parseExpr7: ExprTree = {
      var e6 = parseExpr6
      while(currentToken.kind == OR) {
        val opPos = currentToken
        readToken
        e6 = Or(e6, parseExpr6).setPos(opPos)
      }
      e6
    }
    
    // expr6 ::= expr5 ('&&' expr5)*
    def parseExpr6: ExprTree = {
      var e5 = parseExpr5
      while(currentToken.kind == AND) {
        val opPos = currentToken
        readToken
        e5 = And(e5, parseExpr5).setPos(opPos)
      }
      e5
    }
    
    // expr5 ::= expr4 (('<' | '==') expr4)*
    def parseExpr5: ExprTree = {
      var e4 = parseExpr4
      while(currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        val opPos = currentToken
        if(currentToken.kind == EQUALS) {
          readToken
          e4 = Equals(e4, parseExpr4).setPos(opPos)
        } else {
          readToken
          e4 = LessThan(e4, parseExpr4).setPos(opPos)
        }
      }
      e4
    }
    
    // expr4 ::= expr3 (('+' | '-') expr3)*
    def parseExpr4: ExprTree = {
      var e3 = parseExpr3
      while(currentToken.kind == PLUS || currentToken.kind == MINUS) {
        val opPos = currentToken
        if(currentToken.kind == PLUS) {
          readToken
          e3 = Plus(e3, parseExpr3).setPos(opPos)
        } else {
          readToken
          e3 = Minus(e3, parseExpr3).setPos(opPos)
        }
      }
      e3
    }
    
    // expr3 ::= expr2a (('*' | '/') expr2a)*
    def parseExpr3: ExprTree = {
      var e2 = parseExpr2a
      while(currentToken.kind == DIV || currentToken.kind == TIMES) {
        val opPos = currentToken
        if(currentToken.kind == DIV) {
          readToken
          e2 = Div(e2,parseExpr2a).setPos(opPos)
        } else {
          readToken
          e2 = Times(e2,parseExpr2a).setPos(opPos)
        }
      }
      e2
    }
    
    // expr2a ::= !expr2a | expr2b
    def parseExpr2a: ExprTree = currentToken.kind match {
      case BANG => {
          val pos = currentToken
          readToken
          val expr = parseExpr2a
          Not(expr).setPos(pos)
        }
      case _ => parseExpr2b
    }
    
    // expr2b ::= expr1 (.length | .meth(exprList) | [expr] )*
    def parseExpr2b: ExprTree = {
      var e1 = parseExpr1
      
      while(currentToken.kind == LBRACKET || currentToken.kind == DOT) {
        while(currentToken.kind == LBRACKET) {
          readToken
          val index = parseExpr
          eat(RBRACKET)
          e1 = ArrayRead(e1, index).setPos(e1)
        }
        
        while(currentToken.kind == DOT) {
          readToken
          if(currentToken.kind == LENGTH) {
            val pos = currentToken
            readToken
            e1 = ArrayLength(e1).setPos(pos)
          } else {
            val id = parseIdentifier
            eat(LPAREN)
            val exprLst = parseExprList
            eat(RPAREN)
            e1 = MethodCall(e1, id, exprLst).setPos(id)
          }
        }
      }

      e1
    }

    def parseExpr1: ExprTree = currentToken match {
        case Kinded(TRUE) => { val ret = True().setPos(currentToken); readToken; ret }
        case Kinded(FALSE) => { val ret = False().setPos(currentToken); readToken; ret }
        case Kinded(THIS) => { val ret = This().setPos(currentToken); readToken; ret }
        case nl: INTLIT => { val ret = NumLit(nl.value).setPos(currentToken); readToken; ret }
        case sl: STRLIT => { val ret = StringLit(sl.value).setPos(currentToken); readToken; ret }
        case Kinded(LPAREN) => { readToken; val res = parseExpr; eat(RPAREN); res }
        case id: ID => parseIdentifier
        case Kinded(NEW) => {
          val pos = currentToken
          readToken
          if(currentToken.kind == INT) {
            readToken
            eat(LBRACKET)
            val size = parseExpr
            eat(RBRACKET)
            NewIntArray(size).setPos(pos)
          } else {
            val tpe = parseIdentifier
            eat(LPAREN)
            eat(RPAREN)
            New(tpe).setPos(pos)
          }
        }

        case _ => expected(TRUE,FALSE,THIS,INTLITKIND,STRLITKIND,LPAREN,IDKIND,NEW)
    }

    def parseExprList: List[ExprTree] = {
      if(!exprFirst)
        Nil
      else {
        var lst = parseExpr :: Nil
        while(currentToken.kind == COMMA) {
          readToken
          lst = lst ::: (parseExpr :: Nil)
        }
        lst
      }
    }

    def parseIdentifier: Identifier = currentToken match {
      case i : ID =>
        val ret = Identifier(i.value).setPos(currentToken)
        readToken
        ret

      case _ => expected(IDKIND)
    }

    // checks whether the current token can be the start of an expression.
    def exprFirst: Boolean = currentToken.kind match {
      case IDKIND | INTLITKIND | STRLITKIND | TRUE | FALSE | THIS | NEW | BANG | LPAREN => true
      case _ => false
    }
    
    // checks whether the current token can be the start of a statement
    def statFirst: Boolean = currentToken.kind match {
      case LBRACE | IF | WHILE | PRINTLN | IDKIND => true
      case _ => false
    }

    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
