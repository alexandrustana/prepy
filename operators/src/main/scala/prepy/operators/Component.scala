package prepy.operators

import scala.reflect.macros.blackbox

trait Component {
  def stringify(implicit c: blackbox.Context): String
}
case class Value(value: Any) extends Component {
  override def stringify(implicit c: blackbox.Context): String = value match {
    case _: String | _: Char => s"""'$value'"""
    case e: List[Value] => e.map(_.stringify).mkString(",")
    case e: List[_]     => e.mkString(",")
    case e: Value       => e.value.toString
    case _ => value.toString
  }
}
case class Variable(name: String) extends Component {
  override def stringify(implicit c: blackbox.Context): String = s"$name"
}

abstract class Expression(operand1: Component, operand2: Component, op: String) extends Component {
  override def stringify(implicit c: blackbox.Context): String = {
    s"${operand1.stringify} $op ${operand2.stringify}"
  }
}

abstract class ArithmeticOperation(operand1: Component, operand2: Component, op: String)
    extends Expression(operand1, operand2, op)

case class Add(operand1:      Component, operand2: Component) extends ArithmeticOperation(operand1, operand2, "+")
case class Subtract(operand1: Component, operand2: Component) extends ArithmeticOperation(operand1, operand2, "-")
case class Multiply(operand1: Component, operand2: Component) extends ArithmeticOperation(operand1, operand2, "*")
case class Divide(operand1:   Component, operand2: Component) extends ArithmeticOperation(operand1, operand2, "/")
case class Modulo(operand1:   Component, operand2: Component) extends ArithmeticOperation(operand1, operand2, "+")

abstract class BitwiseOperation(operand1: Component, operand2: Component, op: String)
    extends Expression(operand1, operand2, op)

case class BitAnd private (operand1:      Component, operand2: Component) extends BitwiseOperation(operand1, operand2, "&")
case class BitOr private (operand1:       Component, operand2: Component) extends BitwiseOperation(operand1, operand2, "|")
case class ExclusiveOr private (operand1: Component, operand2: Component)
    extends BitwiseOperation(operand1, operand2, "^")

abstract class ComparisonOperator(operand1: Component, operand2: Component, op: String)
    extends Expression(operand1, operand2, op)

case class Eq(operand1:  Component, operand2: Component) extends ComparisonOperator(operand1, operand2, "=")
case class Neq(operand1: Component, operand2: Component) extends ComparisonOperator(operand1, operand2, "<>")
case class Gt(operand1:  Component, operand2: Component) extends ComparisonOperator(operand1, operand2, ">")
case class Lt(operand1:  Component, operand2: Component) extends ComparisonOperator(operand1, operand2, "<")
case class Gte(operand1: Component, operand2: Component) extends ComparisonOperator(operand1, operand2, ">=")
case class Lte(operand1: Component, operand2: Component) extends ComparisonOperator(operand1, operand2, "<=")

abstract class LogicalOperator(operand1: Component, operand2: Component, op: String) extends Component {
  override def stringify(implicit c: blackbox.Context): String =
    s"(${operand1.stringify} $op ${operand2.stringify})"
}

case class LogicalAnd(operand1: Component, operand2: Component) extends LogicalOperator(operand1, operand2, "AND")
case class LogicalOr(operand1:  Component, operand2: Component) extends LogicalOperator(operand1, operand2, "OR")

case class Like(operand1: Component, operand2: Component) extends Component {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} LIKE ${operand2.stringify}"
}

case class Between(operand1: Component, operand2: Component, operand3: Component) extends Component {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} BETWEEN ${operand2.stringify} AND ${operand3.stringify}"
}
case class In(operand1: Component, operand2: Component) extends Component {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} IN (${operand2.stringify})"
}

case class Not(operand1: Component) extends Component {
  override def stringify(implicit c: blackbox.Context): String = s"!${operand1.stringify}"
}
