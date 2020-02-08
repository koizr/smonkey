package com.koizr.smonkey.lexer

import com.koizr.smonkey.token._

case class Lexer(
                  input: String,
                  position: Int = 0,
                ) {
  private lazy val nextPosition: Int = position + 1
  private lazy val char: Option[Char] = Option.unless(eol)(input.charAt(position))
  private lazy val peekChar: Option[Char] = Option.unless(eol)(input.charAt(nextPosition))
  private lazy val onWhitespace: Boolean = char match {
    case Some(c) =>
      Lexer.whitespaces.contains(c)
    case None => false
  }
  private lazy val onDigit: Boolean = char match {
    case Some(c) => c.toString.toIntOption.nonEmpty
    case None => false
  }
  private lazy val onLetter: Boolean = char match {
    case Some(c) => 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
    case None => false
  }
  private lazy val eol: Boolean = input.length <= position

  private def on(c: Char): Boolean = char match {
    case Some(ch) => ch == c
    case None => false
  }

  private def peekOn(c: Char): Boolean = peekChar match {
    case Some(ch) => ch == c
    case None => false
  }

  private def readChar(): Lexer = Lexer(input, nextPosition)

  @scala.annotation.tailrec
  private def skipWhitespace(): Lexer = if (onWhitespace) readChar().skipWhitespace() else this

  private def readNumber(): (Lexer, String) = Lexer.readWhile(_.onDigit)(this)

  private def readString(): (Lexer, String) = Lexer.readWhile(!_.on('"'))(this.readChar()) match {
    case (l, s) => (l.readChar(), s)
  }

  private def readIdentifier(): (Lexer, String) = Lexer.readWhile(_.onLetter)(this)

  def nextToken(): (Lexer, Token) = {
    val lexer = skipWhitespace()
    lexer.char match {
      case Some(c) => c match {
        // 演算子
        case '=' => if (lexer.peekOn('=')) {
          (lexer.readChar().readChar(), Token(EQToken, "=="))
        } else {
          (lexer.readChar(), Token(AssignToken, "="))
        }
        case '+' => (lexer.readChar(), Token(PlusToken, "+"))
        case '-' => (lexer.readChar(), Token(MinusToken, "-"))
        case '*' => (lexer.readChar(), Token(AsteriskToken, "*"))
        case '/' => (lexer.readChar(), Token(SlashToken, "/"))
        case '<' => (lexer.readChar(), Token(LTToken, "<"))
        case '>' => (lexer.readChar(), Token(GTToken, ">"))
        case '!' => if (lexer.peekOn('=')) {
          (lexer.readChar().readChar(), Token(NEQToken, "!="))
        } else {
          (lexer.readChar(), Token(BangToken, "!"))
        }
        // デリミタ
        case ',' => (lexer.readChar(), Token(CommaToken, ","))
        case ';' => (lexer.readChar(), Token(SemicolonToken, ";"))
        case ':' => (lexer.readChar(), Token(ColonToken, ":"))
        case '(' => (lexer.readChar(), Token(LParenToken, "("))
        case ')' => (lexer.readChar(), Token(RParenToken, ")"))
        case '[' => (lexer.readChar(), Token(LBracketToken, "["))
        case ']' => (lexer.readChar(), Token(RBracketToken, "]"))
        case '{' => (lexer.readChar(), Token(LBraceToken, "{"))
        case '}' => (lexer.readChar(), Token(RBraceToken, "}"))
        // リテラル
        case '"' => lexer.readString() match {
          case (l, str) => (l, Token(StringToken, str))
        }
        case _ if lexer.onLetter => lexer.readIdentifier() match {
          case (l, identifier) => (l, Token(Token.lookupKeywords(identifier), identifier))
        }
        case _ if lexer.onDigit => lexer.readNumber() match {
          case (l, num) => (l, Token(IntegerToken, num))
        }
        // 異常値
        case _ => (lexer.readChar(), Token(IllegalToken, c.toString))
      }
      case None => (lexer.readChar(), Token(EOFToken, ""))
    }
  }
}

object Lexer {
  private val whitespaces: Array[Char] = Array(' ', '\t', '\n', '\r')

  @scala.annotation.tailrec
  private def readWhile(condition: Lexer => Boolean)(lexer: Lexer, chunk: String = ""): (Lexer, String) =
    if (condition(lexer)) {
      // onDigit なので絶対に lexer.char は Some
      readWhile(condition)(lexer.readChar(), chunk :+ lexer.char.get)
    } else {
      (lexer, chunk)
    }
}
