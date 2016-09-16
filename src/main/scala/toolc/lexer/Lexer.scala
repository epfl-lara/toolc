package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "program"  => Some(PROGRAM())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "var"      => Some(VAR())
    case "extends"  => Some(EXTENDS())
    case "Int"      => Some(INT())
    case "Bool"     => Some(BOOLEAN())
    case "String"   => Some(STRING())
    case "while"    => Some(WHILE())
    case "if"       => Some(IF())
    case "else"     => Some(ELSE())
    case "return"   => Some(RETURN())
    case "length"   => Some(LENGTH())
    case "true"     => Some(TRUE())
    case "false"    => Some(FALSE())
    case "this"     => Some(THIS())
    case "new"      => Some(NEW())
    case "println"  => Some(PRINTLN())
    case "do"       => Some(DO())
    case _          => None
  }


  /** Reads the contents of a file, caching two characters at a time.
    * That way we can have a 2-character lookahead with
    * currentChar and nextChar
    */
  private class SourceReader(f: File) {
    private val source = Source.fromFile(f)

    /** We use this character to mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE
    
    private var currentChar_ : Char = _
    private var nextChar_ : Char = _
    private var currentPos_ : Positioned = _
    private var nextPos_ : Positioned = _

    /** The current character */
    def currentChar = currentChar_
    /** The next character */
    def nextChar = nextChar_
    /** The position of the current character */
    def currentPos = currentPos_

    private def readChar(): Char = if (source.hasNext) {
      source.next
    } else {
      EndOfFile
    }

    /** Consumes a character from the input.
      * nextChar becomes currentChar,
      * nextChar points to the first unread character.
      */
    def consume() = {
      currentChar_ = nextChar_
      currentPos_ = nextPos_
      nextChar_ = readChar()
      nextPos_ = new Positioned{}.setPos(f, source.pos)
    }

    /** Consume n characters */
    def consume(n: Int): Unit = for (i <- 1 to n) consume()

    // To start, read the first two characters of the file
    consume(2)
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
        consume(2)
        // Skip until EOL
        while(currentChar != '\n' && currentChar != '\r' && currentChar != EndOfFile) consume()
        nextToken()
      } else if (currentChar == '/' && nextChar == '*') {
        val pos = currentPos
        consume(2)
        // Skip everything until */, fail at EOF
        while (!(currentChar == '*' && nextChar == '/')) {
          if (currentChar == EndOfFile) {
            fatal("unclosed comment", pos)
          } else {
            consume()
          }
        }
        consume(2)
        nextToken()
      } else {
        readToken()
      }
    }

    /** Reads the next token from the stream. */
    def readToken(): Token = {
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
          keywords(str).getOrElse(ID(str)).setPos(tokenPos)

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
          val buffer = new StringBuffer
          consume()
          while (currentChar != '"') {
            if (currentChar == '\n' || currentChar == '\r' || currentChar == EndOfFile) {
              fatal("unclosed string literal", tokenPos)
            }
            buffer.append(currentChar)
            consume()
          }
          consume()
          val str: String = buffer.toString
          STRINGLIT(str).setPos(tokenPos)

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

