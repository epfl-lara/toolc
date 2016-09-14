package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, and one lookahead token, as read from the lexer. */
    var currentToken: Token = BAD()

    def readToken(): Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while(currentToken == BAD()) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected tokens, or terminates with an error. */
    def eat(t1: Token, ts: Token*): Unit = {
      val all = t1 :: ts.toList
      all foreach { t =>
        if(currentToken == t) {
          readToken()
        } else {
          expected(t)
        }
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
    def expected(t1: Token, ts: Token*): Nothing = {
      fatal(
        s"expected: ${(t1 :: ts.toList).map(_.getClass.getSimpleName).mkString(" or ")}," +
        s" found: ${currentToken.getClass.getSimpleName}", currentToken
      )
    }

    def parseGoal: Program = {
      readToken()
      val mainObject = parseMainObject
      var classes: List[ClassDecl] = Nil
      while(currentToken == CLASS()) {
        classes = classes ::: (parseClassDecl :: Nil)
      }
      eat(EOF())
      Program(mainObject, classes).setPos(mainObject)
    }

    def parseMainObject: MainObject = {
      val pos = currentToken
      eat(OBJECT())
      val id = parseIdentifier
      eat(LBRACE(), DEF(), MAIN(), LPAREN(), RPAREN(), COLON(), UNIT(), EQSIGN(), LBRACE())
      val stats = parseStatements
      eat(RBRACE(), RBRACE())
      MainObject(id, stats).setPos(pos)
    }

    def parseClassDecl: ClassDecl = {
      val pos = currentToken
      eat(CLASS())
      val id = parseIdentifier
      val parent = if(currentToken == EXTENDS()) {
        readToken()
        Some(parseIdentifier)
      } else {
        None
      }
      eat(LBRACE())
      val varDecls = parseVarDecls
      var methDecls: List[MethodDecl] = Nil
      while(currentToken == DEF()) {
        methDecls = methDecls ::: (parseMethodDecl :: Nil)
      }
      eat(RBRACE())
      ClassDecl(id, parent, varDecls, methDecls).setPos(pos)
    }

    def parseMethodDecl: MethodDecl = {
      val pos = currentToken
      eat(DEF())
      val id = parseIdentifier
      eat(LPAREN())
      val formals = parseFormals
      eat(RPAREN(), COLON())
      val retTpe = parseType
      eat(EQSIGN(), LBRACE())
      val varDecls = parseVarDecls
      val stats = parseStatements
      eat(RETURN())
      val retExpr = parseExpr
      eat(SEMICOLON(), RBRACE())
      MethodDecl(retTpe, id, formals, varDecls, stats, retExpr).setPos(pos)
    }

    def parseFormals: List[Formal] = {
      if(!currentToken.isInstanceOf[ID])
        Nil
      else {  
        val id = parseIdentifier
        eat(COLON())
        val tpe = parseType

        var lst: List[Formal] = Formal(tpe, id) :: Nil

        while(currentToken == COMMA()) {
          readToken()
          val id2 = parseIdentifier
          eat(COLON())
          val tpe2 = parseType
          lst = Formal(tpe2, id2) :: lst
        }
        lst.reverse
      }
    }

    def parseVarDecls: List[VarDecl] = {
      var lst: List[VarDecl] = Nil
      while(currentToken == VAR()) {
        lst = parseVarDecl :: lst
      }
      lst.reverse
    }

    def parseVarDecl: VarDecl = {
      eat(VAR())
      val id = parseIdentifier
      eat(COLON())
      val tpe = parseType
      eat(SEMICOLON())
      VarDecl(tpe, id).setPos(id)
    }

    def parseType: TypeTree = currentToken match {
      case BOOLEAN() =>
        val ret = BooleanType().setPos(currentToken)
        readToken()
        ret
      case INT() =>
        val pos = currentToken
        readToken()
        if(currentToken == LBRACKET()) {
          readToken()
          eat(RBRACKET())
          IntArrayType().setPos(pos)
        } else {
          IntType().setPos(pos)
        }
      case STRING() =>
        val ret = StringType().setPos(currentToken)
        readToken()
        ret
      case ID(_) => ClassType(parseIdentifier)
      case _ => expected(BOOLEAN(), INT(),STRING(), ID("*"))
    }
    
    def parseStatement: StatTree = currentToken match {
      case LBRACE() =>
        val pos = currentToken
        readToken()
        val ret = Block(parseStatements).setPos(pos)
        eat(RBRACE())
        ret
      case IF() => 
        val pos = currentToken
        readToken()
        eat(LPAREN())
        val cond = parseExpr
        eat(RPAREN())
        val thn = parseStatement
        val elz = if(currentToken == ELSE()) {
          readToken()
          Some(parseStatement)
        } else {
          None
        }
        If(cond, thn, elz).setPos(pos)
      case WHILE() =>
        val pos = currentToken
        readToken()
        eat(LPAREN())
        val cond = parseExpr
        eat(RPAREN())
        While(cond, parseStatement).setPos(pos)
      case PRINTLN() =>
        val pos = currentToken
        readToken()
        eat(LPAREN())
        val expr = parseExpr
        eat(RPAREN(), SEMICOLON())
        Println(expr).setPos(pos)
      case DO() =>
        val pos = currentToken
        readToken()
        eat(LPAREN())
        val expr = parseExpr
        eat(RPAREN(), SEMICOLON())
        DoExpr(expr).setPos(pos)
      case ID(_) =>
        val pos = currentToken
        val id = parseIdentifier
        if(currentToken == EQSIGN()) {
          readToken()
          val expr = parseExpr
          eat(SEMICOLON())
          Assign(id, expr).setPos(pos)
        } else if(currentToken == LBRACKET()) {
          readToken()
          val index = parseExpr
          eat(RBRACKET(), EQSIGN())
          val expr = parseExpr
          eat(SEMICOLON())
          ArrayAssign(id, index, expr).setPos(pos)
        } else {
          expected(EQSIGN(), LBRACKET())
        }
      case _ =>
        expected(LBRACE(), IF(), WHILE(), PRINTLN(), ID("*"))
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
      while(currentToken == OR()) {
        val opPos = currentToken
        readToken()
        e6 = Or(e6, parseExpr6).setPos(opPos)
      }
      e6
    }
    
    // expr6 ::= expr5 ('&&' expr5)*
    def parseExpr6: ExprTree = {
      var e5 = parseExpr5
      while(currentToken == AND()) {
        val opPos = currentToken
        readToken()
        e5 = And(e5, parseExpr5).setPos(opPos)
      }
      e5
    }
    
    // expr5 ::= expr4 (('<' | '==') expr4)*
    def parseExpr5: ExprTree = {
      var e4 = parseExpr4
      while(currentToken == LESSTHAN() || currentToken == EQUALS()) {
        val opPos = currentToken
        if(currentToken == EQUALS()) {
          readToken()
          e4 = Equals(e4, parseExpr4).setPos(opPos)
        } else {
          readToken()
          e4 = LessThan(e4, parseExpr4).setPos(opPos)
        }
      }
      e4
    }
    
    // expr4 ::= expr3 (('+' | '-') expr3)*
    def parseExpr4: ExprTree = {
      var e3 = parseExpr3
      while(currentToken == PLUS() || currentToken == MINUS()) {
        val opPos = currentToken
        if(currentToken == PLUS()) {
          readToken()
          e3 = Plus(e3, parseExpr3).setPos(opPos)
        } else {
          readToken()
          e3 = Minus(e3, parseExpr3).setPos(opPos)
        }
      }
      e3
    }
    
    // expr3 ::= expr2a (('*' | '/') expr2a)*
    def parseExpr3: ExprTree = {
      var e2 = parseExpr2a
      while(currentToken == DIV() || currentToken == TIMES()) {
        val opPos = currentToken
        if(currentToken == DIV()) {
          readToken()
          e2 = Div(e2,parseExpr2a).setPos(opPos)
        } else {
          readToken()
          e2 = Times(e2,parseExpr2a).setPos(opPos)
        }
      }
      e2
    }
    
    // expr2a ::= !expr2a | expr2b
    def parseExpr2a: ExprTree = currentToken match {
      case BANG() =>
        val pos = currentToken
        readToken()
        val expr = parseExpr2a
        Not(expr).setPos(pos)
      case _ => parseExpr2b
    }
    
    // expr2b ::= expr1 (.length | .meth(exprList) | [expr] )*
    def parseExpr2b: ExprTree = {
      var e1 = parseExpr1
      
      while(currentToken == LBRACKET() || currentToken == DOT()) {
        while(currentToken == LBRACKET()) {
          readToken()
          val index = parseExpr
          eat(RBRACKET())
          e1 = ArrayRead(e1, index).setPos(e1)
        }
        
        while(currentToken == DOT()) {
          readToken()
          if(currentToken == LENGTH()) {
            val pos = currentToken
            readToken()
            e1 = ArrayLength(e1).setPos(pos)
          } else {
            val id = parseIdentifier
            eat(LPAREN())
            val exprLst = parseExprList
            eat(RPAREN())
            e1 = MethodCall(e1, id, exprLst).setPos(id)
          }
        }
      }

      e1
    }

    def parseExpr1: ExprTree = currentToken match {
      case TRUE() =>
        val ret = True().setPos(currentToken)
        readToken()
        ret
      case FALSE() =>
        val ret = False().setPos(currentToken)
        readToken()
        ret
      case THIS() =>
        val ret = This().setPos(currentToken)
        readToken()
        ret
      case INTLIT(i) =>
        val ret = IntLit(i).setPos(currentToken)
        readToken()
        ret
      case STRINGLIT(s) =>
        val ret = StringLit(s).setPos(currentToken)
        readToken()
        ret
      case LPAREN() =>
        readToken()
        val res = parseExpr
        eat(RPAREN())
        res
      case id: ID =>
        Variable(parseIdentifier)
      case NEW() =>
        val pos = currentToken
        readToken()
        if(currentToken == INT()) {
          readToken()
          eat(LBRACKET())
          val size = parseExpr
          eat(RBRACKET())
          NewIntArray(size).setPos(pos)
        } else {
          val tpe = parseIdentifier
          eat(LPAREN(), RPAREN())
          New(tpe).setPos(pos)
        }
      case _ =>
        expected(TRUE(), FALSE(), THIS(), INTLIT(0), STRINGLIT(""), LPAREN(), ID("*"), NEW())
    }

    def parseExprList: List[ExprTree] = {
      if(!exprFirst)
        Nil
      else {
        var lst = parseExpr :: Nil
        while(currentToken == COMMA()) {
          readToken()
          lst = lst ::: (parseExpr :: Nil)
        }
        lst
      }
    }

    def parseIdentifier: Identifier = currentToken match {
      case i : ID =>
        val ret = Identifier(i.value).setPos(currentToken)
        readToken()
        ret

      case _ => expected(ID("*"))
    }

    // checks whether the current token can be the start of an expression.
    def exprFirst: Boolean = currentToken match {
      case ID(_) | INTLIT(_) | STRINGLIT(_) | TRUE() | FALSE() | THIS() | NEW() | BANG() | LPAREN() => true
      case _ => false
    }
    
    // checks whether the current token can be the start of a statement
    def statFirst: Boolean = currentToken match {
      case LBRACE() | IF() | WHILE() | PRINTLN() | ID(_) => true
      case _ => false
    }

    val tree = parseGoal
    terminateIfErrors()
    tree
  }
}
