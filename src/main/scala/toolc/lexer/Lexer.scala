package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  val keywords = Map[String, TokenKind](
    "object"  -> OBJECT,
    "class"   -> CLASS,
    "def"     -> DEF,
    "var"     -> VAR,
    "Unit"    -> UNIT,
    "main"    -> MAIN,
    "String"  -> STRING,
    "extends" -> EXTENDS,
    "Int"     -> INT,
    "Bool"    -> BOOLEAN,
    "while"   -> WHILE,
    "if"      -> IF,
    "else"    -> ELSE,
    "return"  -> RETURN,
    "length"  -> LENGTH,
    "true"    -> TRUE,
    "false"   -> FALSE,
    "this"    -> THIS,
    "new"     -> NEW,
    "println" -> PRINTLN
  )


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
        case EndOfFile => new Token(EOF).setPos(tokenPos)

        case _ if Character.isLetter(currentChar) =>
          val buffer = new StringBuffer
          do {
            buffer.append(currentChar)
            consume()
          } while (Character.isLetterOrDigit(currentChar) || currentChar == '_')
          val str: String = buffer.toString
          keywords.get(str) match {
            case Some(tokenInfo) =>
              new Token(tokenInfo).setPos(tokenPos)
            case None =>
              new ID(str).setPos(tokenPos)
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
            new Token(BAD).setPos(tokenPos)
          } else {
            new INTLIT(num.intValue).setPos(tokenPos)
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
          new STRLIT(str).setPos(pos)

        case ':' => consume(); new Token(COLON).setPos(tokenPos)
        case ';' => consume(); new Token(SEMICOLON).setPos(tokenPos)
        case '.' => consume(); new Token(DOT).setPos(tokenPos)
        case ',' => consume(); new Token(COMMA).setPos(tokenPos)
        case '!' => consume(); new Token(BANG).setPos(tokenPos)
        case '(' => consume(); new Token(LPAREN).setPos(tokenPos)
        case ')' => consume(); new Token(RPAREN).setPos(tokenPos)
        case '[' => consume(); new Token(LBRACKET).setPos(tokenPos)
        case ']' => consume(); new Token(RBRACKET).setPos(tokenPos)
        case '{' => consume(); new Token(LBRACE).setPos(tokenPos)
        case '}' => consume(); new Token(RBRACE).setPos(tokenPos)
        case '<' => consume(); new Token(LESSTHAN).setPos(tokenPos)
        case '+' => consume(); new Token(PLUS).setPos(tokenPos)
        case '-' => consume(); new Token(MINUS).setPos(tokenPos)
        case '*' => consume(); new Token(TIMES).setPos(tokenPos)
        case '/' => consume(); new Token(DIV).setPos(tokenPos)
        case '=' =>
          consume()
          if (currentChar == '=') {
            consume()
            new Token(EQUALS).setPos(tokenPos)
          } else {
            new Token(EQSIGN).setPos(tokenPos)
          }
        case '&' =>
          consume()
          if(currentChar == '&') {
            consume()
            new Token(AND).setPos(tokenPos)
          } else {
            error("single '&'", tokenPos)
            new Token(BAD).setPos(tokenPos)
          }
        case '|' =>
          consume()
          if(currentChar == '|') {
            consume()
            new Token(OR).setPos(tokenPos) 
          } else {
            error("single '|'", tokenPos)
            new Token(BAD).setPos(tokenPos)
          }
        case _ =>
          error("invalid character: " + currentChar, tokenPos)
          consume()
          new Token(BAD).setPos(tokenPos)
      }
    }

    new Iterator[Token] {
      var tokenCache: Token = nextToken()
      var reachedEnd = false

      def hasNext = !reachedEnd

      def next = {
        val r = tokenCache
        if (r.kind == EOF) {
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
