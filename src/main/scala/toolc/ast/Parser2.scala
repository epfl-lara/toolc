package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import EBNFGrammar._
import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarReaders.GrammarImplicit
import GrammarDSL._

object Parser2 extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Goal, List[Rules[Token]](
    'Goal ::= 'MainObject ~ 'ClassDecls | 'MainObject,
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
                 | IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'Statement ~ 'ElseOpt
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
    'ExprList ::= 'Expr | 'Expr ~ COMMA() ~ 'ExprList,
    'Op ::= AND() | OR()| EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    ???
  }

}