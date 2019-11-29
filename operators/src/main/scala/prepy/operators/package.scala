package prepy

import scala.annotation.tailrec
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

      case Apply(Select(Apply(Select(Apply(_, List(e)), TermName("between")), List(l)), TermName("and")), List(r)) =>
        Between(getExpression(e), getExpression(l), getExpression(r))
      case Apply(Select(Apply(_, List(a)), (TermName("like"))), List(b)) => Like(getExpression(a), getExpression(b))
      case Apply(Select(Apply(_, List(a)), (TermName("in"))), List(b))   => In(getExpression(a), getExpression(b))

      case Select(Ident(TermName(_)), TermName(x))                        => Variable(x)
      case Apply(TypeApply(Select(Select(_, TermName("List")), _), _), l) => Value(l.map(getExpression))
      case Literal(Constant(v))                                           => Value(v)
      case ref                                                            => Wildcard(getLength(ref))
    }
  }

  @tailrec
  private def getLength(tree: Trees#Tree, acc: Int = 0)(implicit c: blackbox.Context): Int = {
    import c.universe._
    tree match {
      case Select(Ident(TermName(_)), _) => if (acc > 1) acc / 2 else acc
      case Select(inner, _)              => getLength(inner, acc + 1)
      case _                             => if (acc > 1) acc / 2 else acc
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
