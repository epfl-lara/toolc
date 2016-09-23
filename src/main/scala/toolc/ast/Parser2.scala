package toolc
package ast

import grammarcomp.parsing.ParseTreeUtils
import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import grammarcomp.grammar.CFGrammar._
import GrammarDSL._

object Parser2 extends Pipeline[Iterator[Token], Unit] {

  val toolGrammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::=  PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration	::=	CLASS() ~ 'Identifier ~ EXTENDS() ~ 'Identifier ~ 'ClassBody
                        | CLASS() ~ 'Identifier ~ 'ClassBody,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration	::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'Type	::=	INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= LBRACE() ~ 'Stmts ~ RBRACE()
                 | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ 'ElseOpt
                 | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ ELSE()  ~ 'Statement
                 | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
                 | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
                 | 'Identifier ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
                 | 'Identifier ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
                 | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
                  | 'Expression  ~ LBRACKET() ~ 'Expression ~ RBRACKET()
                  | 'Expression ~ DOT() ~ LENGTH()
                  | 'Expression ~ DOT() ~ 'Identifier ~  LPAREN() ~ 'Args ~ RPAREN()
                  | INTLITSENT | STRINGLITSENT
                  | TRUE() | FALSE() | 'Identifier | THIS()
                  | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
                  | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
                  | BANG() ~ 'Expression
                  | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression | 'ExprList,
    'Op ::= AND() | OR()| EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  val precGrammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls ~ EOF() | 'MainObject ~ EOF(),
    'MainObject ::=  PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE()
                  |  PROGRAM() ~ 'Identifier ~ LBRACE() ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | 'Statement,
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | 'ClassDeclaration,
    'ClassDeclaration	::=	CLASS() ~ 'Identifier ~ EXTENDS() ~ 'Identifier ~ 'ClassBody
                        | CLASS() ~ 'Identifier ~ 'ClassBody,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE()
                 | LBRACE() ~ 'VarDecs ~ RBRACE()
                 | LBRACE() ~ 'MethodDecs ~ RBRACE()
                 | LBRACE() ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | 'VarDeclaration,
    'VarDeclaration	::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | 'MethodDeclaration,
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ 'ParamsOpt ~ COLON() ~ 'Type ~ EQSIGN() ~ 'MethodBody ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'ParamsOpt ::= LPAREN() ~ RPAREN() | LPAREN() ~ 'Params ~ RPAREN(),
    'Params ::=  'Param ~ COMMA() ~ 'Params | 'Param,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,
    'MethodBody ::= LBRACE() ~ 'VarDecs ~ 'Stmts
                  | LBRACE() ~ 'VarDecs
                  | LBRACE() ~ 'Stmts
                  | LBRACE(),
    'Type	::=	INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,
    'Statement ::= LBRACE() ~ 'Stmts ~ RBRACE()
                 | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
                 | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ ELSE()  ~ 'Statement
                 | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement
                 | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
                 | 'Identifier ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
                 | 'Identifier ~ LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON()
                 | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'Expression ::= 'Expr7,
    'Expr7 ::= 'Expr6 | 'Expr7 ~ OR() ~ 'Expr6,
    'Expr6 ::= 'Expr5 | 'Expr6 ~ AND() ~ 'Expr5,
    'Expr5 ::= 'Expr4 | 'Expr5 ~ LESSTHAN() ~ 'Expr4 | 'Expr5 ~ EQUALS() ~ 'Expr4,
    'Expr4 ::= 'Expr3 | 'Expr4 ~ PLUS()  ~ 'Expr3 | 'Expr4 ~ MINUS() ~ 'Expr3,
    'Expr3 ::= 'Expr2 | 'Expr3 ~ TIMES() ~ 'Expr2 | 'Expr3 ~ DIV() ~ 'Expr2,
    'Expr2 ::= 'Expr1 | BANG() ~ 'Expr2,
    'Expr1 ::= 'Expr0
             | 'Expr0 ~ DOT() ~ LENGTH()
             | 'Expr0 ~ DOT() ~ 'Identifier ~ 'Args
             | 'Expr0  ~ LBRACKET() ~ 'Expression ~ RBRACKET(),
    'Expr0 ::= INTLITSENT | STRINGLITSENT
             | TRUE() | FALSE()
             | 'Identifier | THIS()
             | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
             | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
             | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= LPAREN() ~ RPAREN() | LPAREN() ~ 'ExprList ~ RPAREN(),
    'ExprList ::= 'Expression | 'Expression ~ COMMA() ~ 'ExprList,
    'Op ::= AND() | OR()| EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    val ptrees = ParseTreeUtils.parseWithTrees(precGrammar, list)
    println(ParseTreeUtils.parseTreetoString(ptrees(0)))
  }

}