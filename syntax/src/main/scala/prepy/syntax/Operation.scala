package prepy.syntax

import spire.algebra.Order
import spire.math._

import scala.reflect.macros.blackbox

trait Component[T] {
  type Out = T

  def stringify(implicit c: blackbox.Context): String
}
case class Value[T](value: T) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String = value.toString
}
case class Variable[T](name: String) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String = {
    import c.universe._
    s"${weakTypeOf[T].typeSymbol.name.toString}.$name"
  }
}

abstract class Expression[I, O](operand1: Component[I], operand2: Component[I], op: String) extends Component[O] {
  override def stringify(implicit c: blackbox.Context): String = {
    s"${operand1.stringify} $op ${operand2.stringify}"
  }
}

abstract class ArithmeticOperation[T: Numeric](operand1: Component[T], operand2: Component[T], op: String)
  extends Expression[T, T](operand1, operand2, op)

case class Add[T: Numeric](operand1: Component[T], operand2: Component[T])
  extends ArithmeticOperation[T](operand1, operand2, "+")
case class Subtract[T: Numeric](operand1: Component[T], operand2: Component[T])
  extends ArithmeticOperation[T](operand1, operand2, "-")
case class Multiply[T: Numeric](operand1: Component[T], operand2: Component[T])
  extends ArithmeticOperation[T](operand1, operand2, "*")
case class Divide[T: Numeric](operand1: Component[T], operand2: Component[T])
  extends ArithmeticOperation[T](operand1, operand2, "/")
case class Modulo[T: Numeric](operand1: Component[T], operand2: Component[T])
  extends ArithmeticOperation[T](operand1, operand2, "+")

abstract class BitwiseOperation[T: Integral](operand1: Component[T], operand2: Component[T], op: String)
  extends Expression[T, T](operand1, operand2, op)

case class BitAnd[T: Integral] private (operand1: Component[T], operand2: Component[T])
  extends BitwiseOperation[T](operand1, operand2, "&")
case class BitOr[T: Integral] private (operand1: Component[T], operand2: Component[T])
  extends BitwiseOperation[T](operand1, operand2, "|")
case class ExclusiveOr[T: Integral] private (operand1: Component[T], operand2: Component[T])
  extends BitwiseOperation[T](operand1, operand2, "^")

abstract class ComparisonOperator[T](operand1: Component[T], operand2: Component[T], op: String)
  extends Expression[T, Boolean](operand1, operand2, op)

case class Eq[T: Order](operand1:  Component[T], operand2: Component[T]) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String = {
    s"${operand1.stringify} = ${operand2.stringify}"
  }
}

case class Gt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator[T](operand1, operand2, ">")
case class Lt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator[T](operand1, operand2, "<")
case class Gte[T](operand1: Component[T], operand2: Component[T])
  extends ComparisonOperator[T](operand1, operand2, ">=")
case class Lte[T](operand1: Component[T], operand2: Component[T])
  extends ComparisonOperator[T](operand1, operand2, "<=")

abstract class LogicalOperator(operand1: Component[Boolean], operand2: Component[Boolean], op: String)
  extends Expression[Boolean, Boolean](operand1, operand2, op)

case class LogicalAnd(operand1: Component[Boolean], operand2: Component[Boolean])
  extends LogicalOperator(operand1, operand2, "AND")
case class LogicalOr(operand1: Component[Boolean], operand2: Component[Boolean])
  extends LogicalOperator(operand1, operand2, "OR")

case class Like[T](operand1: Component[T], operand2: Component[T]) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} LIKE ${operand2.stringify}"
}

case class Between[T](operand1: Component[T], operand2: Component[T], operand3: Component[T]) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} BETWEEN ${operand2.stringify} AND ${operand3.stringify}"
}
case class In[T](operand1: Component[T], operand2: List[Component[T]]) extends Component[T] {
  override def stringify(implicit c: blackbox.Context): String =
    s"${operand1.stringify} IN (${operand2.map(_.stringify).mkString(",")})"
}

case class Not(operand1: Component[Boolean]) extends Component[Boolean] {
  override def stringify(implicit c: blackbox.Context): String = s"!${operand1.stringify}"
}