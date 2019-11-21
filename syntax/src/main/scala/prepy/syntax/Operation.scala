package prepy.syntax

import spire.math._

trait Component[T] {}
case class Constant[T](t:    T)      extends Component[T]
case class Variable[T](name: String) extends Component[T]

trait Expression[I, O] extends Component[I] {}

abstract class ArithmeticOperation[T: Numeric] extends Expression[T, T] {}

case class Add[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Subtract[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Multiply[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Divide[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Modulo[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]

abstract class BitwiseOperation[T: Integral] extends Expression[T, T] {}

case class And[T:         Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]
case class Or[T:          Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]
case class ExclusiveOr[T: Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]

trait ComparisonOperator[T] extends Expression[T, Boolean] {}

case class Eq[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator[T]
case class Gt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator[T]
case class Lt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator[T]
case class Gte[T](operand1: Component[T], operand2: Component[T]) extends ComparisonOperator[T]
case class Lte[T](operand1: Component[T], operand2: Component[T]) extends ComparisonOperator[T]

object Demo {
  Add(Add(Constant(1), Constant(2)), Constant(3))
  And(Constant(1), Constant(2))
  Eq(Eq(Constant(3),  Add(Constant(1), Constant(2))), Constant(1))
  Add(Variable("a"), Constant(3))
}
