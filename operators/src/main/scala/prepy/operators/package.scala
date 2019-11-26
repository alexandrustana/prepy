package prepy

import prepy.operators._

import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

package object operators {

  private def getComponent[T](
                               tree:       Trees#Tree
                             )(implicit c: blackbox.Context): Component = {
    import c.universe._
    tree match {
      case Select(_, TermName(x)) => Variable(x)
      case Literal(Constant(a))   => Value(a)
      case q"$a + $b"             => Add(getComponent(a), getComponent(b))
      case q"$a - $b"             => Subtract(getComponent(a), getComponent(b))
      case q"$a * $b"             => Multiply(getComponent(a), getComponent(b))
      case q"$a / $b"             => Divide(getComponent(a), getComponent(b))
      case q"$a % $b"             => Modulo(getComponent(a), getComponent(b))
      case q"$a == $b"            => Eq(getComponent(a), getComponent(b))
      case q"$a != $b"            => Neq(getComponent(a), getComponent(b))
      case q"$a > $b"             => Gt(getComponent(a), getComponent(b))
      case q"$a >= $b"            => Gte(getComponent(a), getComponent(b))
      case q"$a < $b"             => Lt(getComponent(a), getComponent(b))
      case q"$a <= $b"            => Lte(getComponent(a), getComponent(b))
      case q"$a & $b"             => BitAnd(getComponent(a), getComponent(b))
      case q"$a | $b"             => BitOr(getComponent(a), getComponent(b))
      case q"$a ^ $b"             => ExclusiveOr(getComponent(a), getComponent(b))
      case q"$a && $b"            => LogicalAnd(getComponent(a), getComponent(b))
      case q"$a || $b"            => LogicalOr(getComponent(a), getComponent(b))
    }

  }

  def stringify[T <: Product](f: T => Boolean): String = macro impl[T]

  def impl[T: c.WeakTypeTag](c: blackbox.Context)(f: c.Expr[T => Boolean]): c.Expr[String] = {
    import c.universe._

    val Function(List(ValDef(_, _, _, _)), funcBody) = f.tree

    val components = getComponent[T](funcBody)(c)

    c.Expr[String](q"${components.stringify(c)}")
  }
}
