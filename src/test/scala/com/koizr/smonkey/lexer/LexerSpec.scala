package com.koizr.smonkey.lexer

import com.koizr.smonkey.token._
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {
  "String" should "be lexed to tokens" in {
    val input =
      """
      let five = 5;
      let ten = 10;

      let add = fn(x, y) {
        x + y;
      };

      let result = add(five, ten);

      !-/*5;
      5 < 10 > 5;

      if (5 < 10) {
        return true;
      } else {
        return false;
      }

      10 == 10
      10 != 9
      "foobar"
      "foo bar"

      [1, 2];

      {"foo": "bar"}
      """

    val tests: Array[(TokenType, String)] = Array(
      (LetToken, "let"),
      (IdentifierToken, "five"),
      (AssignToken, "="),
      (IntegerToken, "5"),
      (SemicolonToken, ";"),
      (LetToken, "let"),
      (IdentifierToken, "ten"),
      (AssignToken, "="),
      (IntegerToken, "10"),
      (SemicolonToken, ";"),

      (LetToken, "let"),
      (IdentifierToken, "add"),
      (AssignToken, "="),
      (FunctionToken, "fn"),
      (LParenToken, "("),
      (IdentifierToken, "x"),
      (CommaToken, ","),
      (IdentifierToken, "y"),
      (RParenToken, ")"),
      (LBraceToken, "{"),
      (IdentifierToken, "x"),
      (PlusToken, "+"),
      (IdentifierToken, "y"),
      (SemicolonToken, ";"),
      (RBraceToken, "}"),
      (SemicolonToken, ";"),

      (LetToken, "let"),
      (IdentifierToken, "result"),
      (AssignToken, "="),
      (IdentifierToken, "add"),
      (LParenToken, "("),
      (IdentifierToken, "five"),
      (CommaToken, ","),
      (IdentifierToken, "ten"),
      (RParenToken, ")"),
      (SemicolonToken, ";"),

      (BangToken, "!"),
      (MinusToken, "-"),
      (SlashToken, "/"),
      (AsteriskToken, "*"),
      (IntegerToken, "5"),
      (SemicolonToken, ";"),
      (IntegerToken, "5"),
      (LTToken, "<"),
      (IntegerToken, "10"),
      (GTToken, ">"),
      (IntegerToken, "5"),
      (SemicolonToken, ";"),

      (IfToken, "if"),
      (LParenToken, "("),
      (IntegerToken, "5"),
      (LTToken, "<"),
      (IntegerToken, "10"),
      (RParenToken, ")"),
      (LBraceToken, "{"),
      (ReturnToken, "return"),
      (TrueToken, "true"),
      (SemicolonToken, ";"),
      (RBraceToken, "}"),
      (ElseToken, "else"),
      (LBraceToken, "{"),
      (ReturnToken, "return"),
      (FalseToken, "false"),
      (SemicolonToken, ";"),
      (RBraceToken, "}"),

      (IntegerToken, "10"),
      (EQToken, "=="),
      (IntegerToken, "10"),
      (IntegerToken, "10"),
      (NEQToken, "!="),
      (IntegerToken, "9"),

      (StringToken, "foobar"),
      (StringToken, "foo bar"),

      (LBracketToken, "["),
      (IntegerToken, "1"),
      (CommaToken, ","),
      (IntegerToken, "2"),
      (RBracketToken, "]"),
      (SemicolonToken, ";"),

      (LBraceToken, "{"),
      (StringToken, "foo"),
      (ColonToken, ":"),
      (StringToken, "bar"),
      (RBraceToken, "}"),

      (EOFToken, "")
    )

    var lexer = Lexer(input)
    for {(tokenType, literal) <- tests} lexer.nextToken() match {
      case (l, t) =>
        lexer = l
        t._type shouldBe tokenType
        t.literal shouldBe literal
    }
  }

  "String literal" should "be lexed to StringToken" in {
    Lexer("\"foo bar\"").nextToken() match {
      case (_, token) => token.literal shouldBe "foo bar"
    }
  }
}
