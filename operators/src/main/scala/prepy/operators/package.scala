package prepy

import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

package object operators {

  private def getExpression[T](
    tree:       Trees#Tree
  )(implicit c: blackbox.Context): Operator = {
    import c.universe._
    tree match {
      case q"$a + $b" => Add(getExpression(a), getExpression(b))
      case q"$a - $b" => Subtract(getExpression(a), getExpression(b))
      case q"$a * $b" => Multiply(getExpression(a), getExpression(b))
      case q"$a / $b" => Divide(getExpression(a), getExpression(b))
      case q"$a % $b" => Modulo(getExpression(a), getExpression(b))

      case q"$a == $b" => Eq(getExpression(a), getExpression(b))
      case q"$a != $b" => Neq(getExpression(a), getExpression(b))
      case q"$a > $b"  => Gt(getExpression(a), getExpression(b))
      case q"$a >= $b" => Gte(getExpression(a), getExpression(b))
      case q"$a < $b"  => Lt(getExpression(a), getExpression(b))
      case q"$a <= $b" => Lte(getExpression(a), getExpression(b))

      case q"$a & $b" => BitAnd(getExpression(a), getExpression(b))
      case q"$a | $b" => BitOr(getExpression(a), getExpression(b))
      case q"$a ^ $b" => ExclusiveOr(getExpression(a), getExpression(b))

      case q"$a && $b" => LogicalAnd(getExpression(a), getExpression(b))
      case q"$a || $b" => LogicalOr(getExpression(a), getExpression(b))

      case Select(Ident(TermName(_)), TermName(x))                        => Variable(x)
      case ref @ Select(_, TermName(a))                                   => Value(c.eval(c.Expr[T](c.untypecheck(ref.duplicate))))
      case Apply(TypeApply(Select(Select(_, TermName("List")), _), _), l) => Value(l.map(getExpression))
      case Literal(Constant(a))                                           => Value(a)

      case Apply(Select(Apply(Select(Apply(_, List(e)), TermName("between")), List(l)), TermName("and")), List(r)) =>
        Between(getExpression(e), getExpression(l), getExpression(r))
      case Apply(Select(Apply(_, List(a)), (TermName("like"))), List(b)) => Like(getExpression(a), getExpression(b))
      case Apply(Select(Apply(_, List(a)), (TermName("in"))), List(b))   => In(getExpression(a), getExpression(b))
    }
  }

  def stringify[T <: Product](f: T => Boolean): String = macro impl[T]

  def impl[T: c.WeakTypeTag](c: blackbox.Context)(f: c.Expr[T => Boolean]): c.Expr[String] = {
    import c.universe._

    val Function(List(ValDef(_, _, _, _)), funcBody) = f.tree

    val expression = getExpression[T](funcBody)(c)

    c.Expr[String](q"${expression.stringify}")
  }
}
