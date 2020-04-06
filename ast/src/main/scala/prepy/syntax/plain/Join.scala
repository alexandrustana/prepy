package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.internal.Codec
import shapeless.Coproduct
import shapeless.ops.coproduct.Selector

object Join {
  import prepy.syntax.plain.operators.getExpression
  import scala.reflect.macros.blackbox
  import scala.language.experimental.macros

  def stringifyPredicate[T <: Product: c.WeakTypeTag, P <: Product: c.WeakTypeTag](
    c:         blackbox.Context
  )(predicate: c.Expr[(T, P) => Boolean]): c.Expr[String] = {
    import c.universe._

    val serializedPredicate: c.Expr[String] = predicate.tree match {
      case q"(..$p) => $body" => c.Expr[String](q"${getExpression[T](body)(c).stringify}")
    }

    serializedPredicate
  }

  def impl[From <: Product: c.WeakTypeTag, To <: Product: c.WeakTypeTag, Tables <: Coproduct: c.WeakTypeTag](
    c:         blackbox.Context
  )(predicate: c.Expr[(From, To) => Boolean])(toSelector: c.Expr[Selector[Tables, To]]): c.Expr[`onT`[Tables]] = {
    import c.universe._

    reify {
      `onT`[Tables](c.prefix.splice.asInstanceOf[Query].toString, stringifyPredicate(c)(predicate).splice)
    }
  }

  case class `joinT`[From, Tables <: Coproduct](queryElement: Query, tableName: String) extends Query {

    override def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
      Invalid("Incomplete SQL query. `join[T]` must be followed by a `on((t, k) => true)`")

    def on[To <: Product](
      predicate:           (From, To) => Boolean
    )(implicit toSelector: Selector[Tables, To]): `onT`[Tables] =
      macro impl[From, To, Tables]

    override def toString: String = s"$queryElement INNER JOIN ${Codec.encode(tableName)}"
  }

  case class `onT`[Tables <: Coproduct](queryElement: String, condition: String) extends Query {

    override def toString: String =
      s"$queryElement ON (${condition})"
  }
}
