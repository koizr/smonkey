package com.koizr.smonkey.parser

import com.koizr.smonkey.ast._
import com.koizr.smonkey.lexer.Lexer
import com.koizr.smonkey.token._

object Precedence {
  val lowest = 1
  val equals = 2
  val lessGreater = 3
  val sum = 4
  val product = 5
  val prefix = 6
  val call = 7
}

object Precedences {
  private val map = Map(
    EQToken -> Precedence.equals,
    NEQToken -> Precedence.equals,
    LTToken -> Precedence.lessGreater,
    GTToken -> Precedence.lessGreater,
    PlusToken -> Precedence.sum,
    MinusToken -> Precedence.sum,
    SlashToken -> Precedence.product,
    AsteriskToken -> Precedence.product,
    LParenToken -> Precedence.call,
  )

  def lookup(token: TokenType): Int = map.getOrElse(token, Precedence.lowest)
}

class Parser(var lexer: Lexer,
             var curToken: Token,
             var peekToken: Token
            ) {

  def parse(): Either[Seq[ParseError], Program] = {
    var statements: Seq[Statement] = Seq.empty
    var errors: Seq[ParseError] = Seq.empty
    while (curToken.tokenType != EOFToken) {
      parseStatement() match {
        case Right(statement) => statements = statements :+ statement
        case Left(error) => errors = errors :+ error
      }
    }
    if (errors.isEmpty) Right(Program(statements)) else Left(errors)
  }

  private def parseStatement(): Either[ParseError, Statement] = curToken.tokenType match {
    case LetToken => parseLetStatement()
    case ReturnToken => parseReturnStatement()
    case _ => parseExpressionStatement()
  }

  private def parseLetStatement(): Either[ParseError, LetStatement] = {
    val letToken = curToken
    forwardToken()
    for {
      identifier <- parseIdentifier()
      _ <- requireToken(AssignToken)
      value <- parseExpression()
      _ = skipPeekToken(SemicolonToken)
    } yield LetStatement(letToken, identifier, value)
  }

  private def parseReturnStatement(): Either[ParseError, ReturnStatement] = ???

  private def parseExpressionStatement(): Either[ParseError, ExpressionStatement] = {
    val token = curToken
    for {
      expression <- parseExpression()
      _ = skipPeekToken(SemicolonToken)
    } yield ExpressionStatement(token, expression)
  }

  private def parseExpression(): Either[ParseError, Expression] = ???

  private def parseIdentifier(): Either[ParseError, Identifier] =
    curToken.tokenType match {
      case IdentifierToken =>
        val identifier = Identifier(Token(curToken.tokenType, curToken.literal), curToken.literal)
        forwardToken()
        Right(identifier)
      case _ => Left(NotExpectedToken(IdentifierToken, curToken.tokenType))
    }

  private def forwardToken(): Unit = {
    lexer.nextToken() match {
      case (nextLexer, token) =>
        lexer = nextLexer
        curToken = peekToken
        peekToken = token
    }
  }

  private def expectCurrent(expected: TokenType): Boolean = curToken.tokenType == expected

  private def expectPeek(expected: TokenType): Boolean = peekToken.tokenType == expected

  private def skipPeekToken(expected: TokenType): Unit =
    if (expectCurrent(expected)) {
      forwardToken()
    }

  private def requireToken(token: TokenType): Either[ParseError, Token] =
    if (expectCurrent(token)) {
      val requiredToken = curToken
      forwardToken()
      Right(requiredToken)
    } else {
      Left(NotExpectedToken(token, curToken.tokenType))
    }
}

object Parser {
  def apply(lexer: Lexer): Parser = {
    // トークンを 2 回読み進めて curToken, nextToken を埋める
    lexer.nextToken() match {
      case (lexer, curToken) => lexer.nextToken() match {
        case (lexer, nextToken) => new Parser(lexer, curToken, nextToken)
      }
    }
  }
}

sealed trait ParseError {
  def message(): String
}

case class NotExpectedToken(expected: TokenType, actual: TokenType) extends ParseError {
  override def message(): String = s"expected token is ${expected}, got ${actual} instead."
}

