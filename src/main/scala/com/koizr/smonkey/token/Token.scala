package com.koizr.smonkey.token

sealed trait TokenType

object IllegalToken extends TokenType

object EOFToken extends TokenType

// リテラル

object IdentifierToken extends TokenType

object IntegerToken extends TokenType

object StringToken extends TokenType

// 演算子

object AssignToken extends TokenType

object PlusToken extends TokenType

object MinusToken extends TokenType

object AsteriskToken extends TokenType

object SlashToken extends TokenType

object BangToken extends TokenType

object LTToken extends TokenType

object GTToken extends TokenType

object EQToken extends TokenType

object NEQToken extends TokenType

// デリミタ

object CommaToken extends TokenType

object SemicolonToken extends TokenType

object ColonToken extends TokenType

object LParenToken extends TokenType

object RParenToken extends TokenType

object LBraceToken extends TokenType

object RBraceToken extends TokenType

object LBracketToken extends TokenType

object RBracketToken extends TokenType

// キーワード

object FunctionToken extends TokenType

object LetToken extends TokenType

object TrueToken extends TokenType

object FalseToken extends TokenType

object IfToken extends TokenType

object ElseToken extends TokenType

object ReturnToken extends TokenType


case class Token(tokenType: TokenType, literal: String)

object Token {
  private val keywords: Map[String, TokenType] = Map(
    ("fn", FunctionToken),
    ("let", LetToken),
    ("true", TrueToken),
    ("false", FalseToken),
    ("if", IfToken),
    ("else", ElseToken),
    ("return", ReturnToken),
  )

  def lookupKeywords(id: String): TokenType = keywords.get(id) match {
    case Some(tokenType) => tokenType
    case None => IdentifierToken
  }
}
