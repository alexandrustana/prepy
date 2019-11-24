package prepy.syntax

import spire.math._

trait Component[T] {
  type Out = T
}
case class Constant[T](t:    T)      extends Component[T]
case class Variable[T](name: String) extends Component[T]

trait Expression[I, O] extends Component[I] {}

abstract class ArithmeticOperation[T: Numeric] extends Component[T] {}

case class Add[T:      Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Subtract[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Multiply[T: Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Divide[T:   Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]
case class Modulo[T:   Numeric](operand1: Component[T], operand2: Component[T]) extends ArithmeticOperation[T]

abstract class BitwiseOperation[T: Integral] extends Component[T] {}

case class BitAnd[T:         Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]
case class BitOr[T:          Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]
case class ExclusiveOr[T: Integral] private (operand1: Component[T], operand2: Component[T]) extends BitwiseOperation[T]

trait ComparisonOperator extends Component[Boolean] {}

case class Eq[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator
case class Gt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator
case class Lt[T](operand1:  Component[T], operand2: Component[T]) extends ComparisonOperator
case class Gte[T](operand1: Component[T], operand2: Component[T]) extends ComparisonOperator
case class Lte[T](operand1: Component[T], operand2: Component[T]) extends ComparisonOperator

trait LogicalOperator extends Component[Boolean] {}

case class LogicalAnd[T](operand1:     Component[T], operand2: Component[T]) extends LogicalOperator
case class LogicalOr[T](operand1:      Component[T], operand2: Component[T]) extends LogicalOperator
case class Between[T](operand1: Component[T], operand2: Component[T], operand3: Component[T]) extends LogicalOperator
case class In[T](operand1:      Component[T], operand2: List[Component[T]]) extends LogicalOperator
case class Like[T](operand1:    Component[T], operand2: Component[T]) extends LogicalOperator
case class Not[T](operand1:     Component[T], operand2: Component[T]) extends LogicalOperator