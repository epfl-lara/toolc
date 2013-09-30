package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  val keywords = Map[String, TokenKind](
    ("object" -> OBJECT),
    ("class" -> CLASS),
    ("def" -> DEF),
    ("var" -> VAR),
    ("Unit" -> UNIT),
    ("main" -> MAIN),
    ("String" -> STRING),
    ("extends" -> EXTENDS),
    ("Int" -> INT),
    ("Bool" -> BOOLEAN),
    ("while" -> WHILE),
    ("if" -> IF),
    ("else" -> ELSE),
    ("return" -> RETURN),
    ("length" -> LENGTH),
    ("true" -> TRUE),
    ("false" -> FALSE),
    ("this" -> THIS),
    ("new" -> NEW),
    ("println" -> PRINTLN))

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    // the last char seen in the input stream
    var currentChar: Char = '\0'
    /** Used to detect \r\n pairs and ouput only \n for them. */
    var previousChar: Char = '\0'

    // the position of the beginning of the current token
    var tokenPos: Positioned = NoPosition

    def currentPos(): Positioned = {
      new Positioned{}.setPos(f, source.pos)
    }

    // a buffer to store textual information for parametric tokens
    val buffer: StringBuffer = new StringBuffer

    /** What will mark the end of the input stream. */
    val EndOfFile: Char = java.lang.Character.MAX_VALUE

    /** Puts the next character in the input stream in currentChar, or the special character EndOfFile if the stream is exhausted. */
    def nextChar: Unit = {
      def readChar: Char = if(source.hasNext) {
        source.next
      } else {
        EndOfFile
      }

      currentChar match {
        case EndOfFile => return
        case _ => ; 
      }

      currentChar  = readChar
      previousChar = if ((previousChar == '\r') && (currentChar == '\n')) readChar else currentChar
      currentChar = if (previousChar == '\r') '\n' else previousChar
    }

    /** Gets rid of whitespaces and comments and calls readToken to get the next token. */
    def nextToken: Token = {

      while(currentChar == '/' || Character.isWhitespace(currentChar)) {
        if(currentChar == '/') {
          tokenPos = currentPos()
          nextChar

          if(currentChar == '/') {
            // skips end-of-line comments
            while(currentChar != '\n' && currentChar != EndOfFile) nextChar
          } else if(currentChar == '*') {
            // skips block comments
            var foundEnd: Boolean = false
            while(!foundEnd) {
              while(currentChar != '*') {
                if(currentChar == EndOfFile) fatal("unterminated block comment", currentPos())
                  nextChar
              }
              nextChar
              if(currentChar == '/') { foundEnd = true; nextChar }
            }
          } else {
            return new Token(DIV).setPos(tokenPos)
          }
        } else {
          nextChar
        }
      }

      readToken
    }

    /** Reads the next token from the stream. */
    def readToken: Token = {
      tokenPos = currentPos()

      currentChar match {
        case EndOfFile => new Token(EOF).setPos(tokenPos)

        case _ if Character.isLetter(currentChar) => {
          buffer.setLength(0)
          do {
            buffer.append(currentChar)
            nextChar
          } while (Character.isLetterOrDigit(currentChar) || currentChar == '_')
          val str: String = buffer.toString
          keywords.get(str) match {
            case Some(tokenInfo) =>
              new Token(tokenInfo).setPos(tokenPos)
            case None =>
              new ID(str).setPos(tokenPos)
          }
        }

        case '0' => { nextChar; new INTLIT(0).setPos(tokenPos) }

        case _ if Character.isDigit(currentChar) => {
          buffer.setLength(0)
          do {
            buffer.append(currentChar)
            nextChar
          } while (Character.isDigit(currentChar))
          val num = scala.math.BigInt(buffer.toString)
          if(!num.isValidInt) {
            error("value out of integer range", tokenPos)
            new Token(BAD).setPos(tokenPos)
          } else {
            new INTLIT(num.intValue).setPos(tokenPos)
          }
        }

        case '"' => {
          buffer.setLength(0)
          nextChar
          while (currentChar != '"') {
            if (currentChar == '\n' || currentChar == EndOfFile) {
              fatal("unterminated string", tokenPos)
            }
            buffer.append(currentChar)
            nextChar
          }
          nextChar
          val str: String = buffer.toString
          new STRLIT(str).setPos(tokenPos)
        }

        case ':' => { nextChar; new Token(COLON).setPos(tokenPos) }
        case ';' => { nextChar; new Token(SEMICOLON).setPos(tokenPos) }
        case '.' => { nextChar; new Token(DOT).setPos(tokenPos) }
        case ',' => { nextChar; new Token(COMMA).setPos(tokenPos) }
        case '!' => { nextChar; new Token(BANG).setPos(tokenPos) }
        case '(' => { nextChar; new Token(LPAREN).setPos(tokenPos) }
        case ')' => { nextChar; new Token(RPAREN).setPos(tokenPos) }
        case '[' => { nextChar; new Token(LBRACKET).setPos(tokenPos) }
        case ']' => { nextChar; new Token(RBRACKET).setPos(tokenPos) }
        case '{' => { nextChar; new Token(LBRACE).setPos(tokenPos) }
        case '}' => { nextChar; new Token(RBRACE).setPos(tokenPos) }
        case '<' => { nextChar; new Token(LESSTHAN).setPos(tokenPos) }
        case '+' => { nextChar; new Token(PLUS).setPos(tokenPos) }
        case '-' => { nextChar; new Token(MINUS).setPos(tokenPos) }
        case '*' => { nextChar; new Token(TIMES).setPos(tokenPos) }
        case '=' => { nextChar; if(currentChar == '=') {
                                  nextChar; new Token(EQUALS).setPos(tokenPos)
                                } else {
                                  new Token(EQSIGN).setPos(tokenPos)
                                }
                    }
        case '&' => { nextChar; if(currentChar == '&') {
                                  nextChar; new Token(AND).setPos(tokenPos)
                                } else {
                                  error("single '&'", tokenPos); new Token(BAD).setPos(tokenPos)
                                }
                    }
        case '|' => { nextChar; if(currentChar == '|') {
                                  nextChar; new Token(OR).setPos(tokenPos) 
                                } else {
                                  error("single '|'", tokenPos); new Token(BAD).setPos(tokenPos)
                                }
                    }

        case _ => {
          error("invalid character: " + currentChar, currentPos());
          nextChar;
          new Token(BAD).setPos(tokenPos) }
      }
    }

    nextChar

    new Iterator[Token] {
      var tokenCache: Token = nextToken
      var reachedEnd = false

      def hasNext = {
        tokenCache.kind != EOF || !reachedEnd
      }

      def next = {
        val r = tokenCache
        tokenCache = nextToken
        if (r.kind == EOF) {
          reachedEnd = true
        }
        r
      }
    }
  }
}
