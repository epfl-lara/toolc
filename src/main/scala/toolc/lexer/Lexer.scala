package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  val keywords: PartialFunction[String, Token] = {
    case "program"  => PROGRAM()
    case "class"    => CLASS()
    case "def"      => DEF()
    case "var"      => VAR()
    case "String"   => STRING()
    case "extends"  => EXTENDS()
    case "Int"      => INT()
    case "Bool"     => BOOLEAN()
    case "while"    => WHILE()
    case "if"       => IF()
    case "else"     => ELSE()
    case "return"   => RETURN()
    case "length"   => LENGTH()
    case "true"     => TRUE()
    case "false"    => FALSE()
    case "this"     => THIS()
    case "new"      => NEW()
    case "println"  => PRINTLN()
    case "do"       => DO()
  }


  /** Reads the contents of a file, caching two characters at a time */
  class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** What will mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    def currentChar = currentChar_
    def nextChar = nextChar_
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      // Skip all '\r's
      while (nextChar_ == '\r') nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    consume()
    consume()

  }


  def run(ctx: Context)(f: File): Iterator[Token] = {
    import ctx.reporter._

    val reader = new SourceReader(f)
    import reader._

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    @scala.annotation.tailrec
    def nextToken(): Token = {
      while (Character.isWhitespace(currentChar)) {
        consume()
      }
      if (currentChar == '/' && nextChar == '/') {
        consume()
        consume()
        // Skip until EOL
        while(currentChar != '\n' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        val pos = currentPos
        consume()
        consume()
        // Skip everything until */, fail at EOF
        while (!(currentChar == '*' && nextChar == '/')) {
          if (currentChar == EndOfFile) {
            fatal("unclosed comment", pos)
          } else {
            consume()
          }
        }
        consume()
        consume()
        nextToken()
      } else {
        readToken
      }
    }

    /** Reads the next token from the stream. */
    def readToken: Token = {
      val tokenPos = currentPos

      currentChar match {
        case EndOfFile => EOF().setPos(tokenPos)

        case _ if Character.isLetter(currentChar) =>
          val buffer = new StringBuffer
          do {
            buffer.append(currentChar)
            consume()
          } while (Character.isLetterOrDigit(currentChar) || currentChar == '_')
          val str: String = buffer.toString
          keywords.lift(str) match {
            case Some(token) =>
              token.setPos(tokenPos)
            case None =>
              ID(str).setPos(tokenPos)
          }

        case _ if Character.isDigit(currentChar) =>
          val buffer = new StringBuffer
          do {
            buffer.append(currentChar)
            consume()
          } while (Character.isDigit(currentChar))
          val num = scala.math.BigInt(buffer.toString)
          if(!num.isValidInt) {
            error("value out of integer range", tokenPos)
            BAD().setPos(tokenPos)
          } else {
            INTLIT(num.intValue).setPos(tokenPos)
          }

        case '"' =>
          val pos = currentPos
          val buffer = new StringBuffer
          consume()
          while (currentChar != '"') {
            if (currentChar == '\n' || currentChar == EndOfFile) {
              fatal("unclosed string literal", pos)
            }
            buffer.append(currentChar)
            consume()
          }
          consume()
          val str: String = buffer.toString
          STRINGLIT(str).setPos(pos)

        case ':' => consume(); COLON().setPos(tokenPos)
        case ';' => consume(); SEMICOLON().setPos(tokenPos)
        case '.' => consume(); DOT().setPos(tokenPos)
        case ',' => consume(); COMMA().setPos(tokenPos)
        case '!' => consume(); BANG().setPos(tokenPos)
        case '(' => consume(); LPAREN().setPos(tokenPos)
        case ')' => consume(); RPAREN().setPos(tokenPos)
        case '[' => consume(); LBRACKET().setPos(tokenPos)
        case ']' => consume(); RBRACKET().setPos(tokenPos)
        case '{' => consume(); LBRACE().setPos(tokenPos)
        case '}' => consume(); RBRACE().setPos(tokenPos)
        case '<' => consume(); LESSTHAN().setPos(tokenPos)
        case '+' => consume(); PLUS().setPos(tokenPos)
        case '-' => consume(); MINUS().setPos(tokenPos)
        case '*' => consume(); TIMES().setPos(tokenPos)
        case '/' => consume(); DIV().setPos(tokenPos)
        case '=' =>
          consume()
          if (currentChar == '=') {
            consume()
            EQUALS().setPos(tokenPos)
          } else {
            EQSIGN().setPos(tokenPos)
          }
        case '&' =>
          consume()
          if(currentChar == '&') {
            consume()
            AND().setPos(tokenPos)
          } else {
            error("single '&'", tokenPos)
            BAD().setPos(tokenPos)
          }
        case '|' =>
          consume()
          if(currentChar == '|') {
            consume()
            OR().setPos(tokenPos)
          } else {
            error("single '|'", tokenPos)
            BAD().setPos(tokenPos)
          }
        case _ =>
          error("invalid character: " + currentChar, tokenPos)
          consume()
          BAD().setPos(tokenPos)
      }
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r == EOF()) {
          reachedEnd = true
        } else {
          tokenCache = nextToken()
        }
        r
      }
    }
  }
}

object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    val l = tokens.toList
    l foreach { t => println(s"$t(${t.line}:${t.col})") }
    l.iterator
  }
}
