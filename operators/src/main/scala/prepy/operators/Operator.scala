package prepy.operators

import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter

trait Operator {
  def stringify(implicit formatter: Formatter = IdentityFormatter): String
}
case class Value(value: Any) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String = value match {
    case _: String | _: Char => s"""'${value.toString}'"""
    case e: List[_] => e.map(_.toString).mkString(",")
    case _ => value.toString
  }

  override def toString: String = value match {
    case _: Boolean => value.toString.toUpperCase
    case _: Char | _: String => s"""'${value.toString}'"""
    case _ => value.toString
  }
}
case class Variable(name: String) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter) = s"${formatter(name)}"

  override def toString: String = name
}

abstract class Expression(operand1: Operator, operand2: Operator, op: String) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String = {
    s"${operand1.stringify} $op ${operand2.stringify}"
  }
}

abstract class ArithmeticOperation(operand1: Operator, operand2: Operator, op: String)
    extends Expression(operand1, operand2, op)

case class Add(operand1:      Operator, operand2: Operator) extends ArithmeticOperation(operand1, operand2, "+")
case class Subtract(operand1: Operator, operand2: Operator) extends ArithmeticOperation(operand1, operand2, "-")
case class Multiply(operand1: Operator, operand2: Operator) extends ArithmeticOperation(operand1, operand2, "*")
case class Divide(operand1:   Operator, operand2: Operator) extends ArithmeticOperation(operand1, operand2, "/")
case class Modulo(operand1:   Operator, operand2: Operator) extends ArithmeticOperation(operand1, operand2, "MOD")

abstract class BitwiseOperation(operand1: Operator, operand2: Operator, op: String)
    extends Expression(operand1, operand2, op)

case class BitAnd private (operand1:      Operator, operand2: Operator) extends BitwiseOperation(operand1, operand2, "&")
case class BitOr private (operand1:       Operator, operand2: Operator) extends BitwiseOperation(operand1, operand2, "|")
case class ExclusiveOr private (operand1: Operator, operand2: Operator)
    extends BitwiseOperation(operand1, operand2, "^")

abstract class ComparisonOperator(operand1: Operator, operand2: Operator, op: String)
    extends Expression(operand1, operand2, op)

case class Eq(operand1:  Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, "=")
case class Neq(operand1: Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, "<>")
case class Gt(operand1:  Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, ">")
case class Lt(operand1:  Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, "<")
case class Gte(operand1: Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, ">=")
case class Lte(operand1: Operator, operand2: Operator) extends ComparisonOperator(operand1, operand2, "<=")

abstract class LogicalOperator(operand1: Operator, operand2: Operator, op: String) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String =
    s"(${operand1.stringify} $op ${operand2.stringify})"
}

case class LogicalAnd(operand1: Operator, operand2: Operator) extends LogicalOperator(operand1, operand2, "AND")
case class LogicalOr(operand1:  Operator, operand2: Operator) extends LogicalOperator(operand1, operand2, "OR")

case class Like(operand1: Operator, operand2: Operator) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String =
    s"${operand1.stringify} LIKE ${operand2.stringify}"
}

case class Between(operand1: Operator, operand2: Operator, operand3: Operator) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String =
    s"${operand1.stringify} BETWEEN ${operand2.stringify} AND ${operand3.stringify}"
}
case class In(operand1: Operator, operand2: Operator) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String =
    s"${operand1.stringify} IN (${operand2.stringify})"
}

case class Not(operand1: Operator) extends Operator {
  override def stringify(implicit formatter: Formatter = IdentityFormatter): String = s"!${operand1.stringify}"
}
