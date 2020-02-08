package com.koizr.smonkey.ast

import com.koizr.smonkey.token._
import org.scalatest._

class AstSpec extends FlatSpec with Matchers {
  "Program ast" should "be string" in {
    Program(Array(
      LetStatement(
        Token(LetToken, "let"),
        Identifier(Token(IdentifierToken, "age"), "age"),
        IntegerLiteral(Token(IntegerToken, "20"), 20)
      ),
      ExpressionStatement(
        Token(IdentifierToken, "age"),
        InfixExpression(
          Token(PlusToken, "+"),
          Identifier(Token(IdentifierToken, "age"), "age"),
          "+",
          IntegerLiteral(Token(IntegerToken, "10"), 10)
        )
      )
    )).represent() shouldBe "let age = 20;\n(age + 10);"
  }
}
