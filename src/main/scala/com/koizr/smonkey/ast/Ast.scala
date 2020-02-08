package com.koizr.smonkey.ast

import com.koizr.smonkey.token._


sealed trait Node {
  def tokenLiteral(): String

  def represent(): String
}

sealed trait Statement extends Node

sealed trait Expression extends Node

case class Program(statements: Seq[Statement]) extends Node {
  override def tokenLiteral(): String = statements.map(_.tokenLiteral()).mkString("\n")

  override def represent(): String = statements.map(_.represent()).mkString("\n")
}

case class LetStatement(token: Token, name: Identifier, value: Expression) extends Statement {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"${token.literal} ${name.represent()} = ${value.represent()};"
}

case class ReturnStatement(token: Token, value: Expression) extends Statement {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"${token.literal} ${value.represent()};"
}

case class ExpressionStatement(token: Token, expression: Expression) extends Statement {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"${expression.represent()};"
}

case class BlockStatement(token: Token, statements: Seq[Statement]) extends Statement {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = statements.map(_.represent()).mkString(";") + ";"
}

case class Identifier(token: Token, value: String) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = value
}

case class BooleanLiteral(token: Token, value: Boolean) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = value.toString
}

case class IntegerLiteral(token: Token, value: Int) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = value.toString
}

case class StringLiteral(token: Token, value: String) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = value
}

case class PrefixExpression(token: Token, operator: String, operand: Expression) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"(${operator}${operand.represent()})"
}

case class InfixExpression(token: Token, left: Expression, operator: String, right: Expression) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"(${left.represent()} ${operator} ${right.represent()})"
}

case class IfExpression(token: Token,
                        condition: Expression,
                        consequence: BlockStatement,
                        alternative: BlockStatement
                       ) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"if (${condition.represent()}) ${consequence.represent()} else ${alternative.represent()}"
}

case class FunctionLiteral(token: Token, params: Seq[Identifier], body: BlockStatement) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"${token.literal}(${params.map(_.represent()).mkString(", ")}) ${body.represent()}"
}

case class CallExpression(token: Token, function: Expression, args: Seq[Expression]) extends Expression {
  override def tokenLiteral(): String = token.literal

  override def represent(): String = s"${function.represent()}(${args.map(_.represent()).mkString(", ")})"
}
