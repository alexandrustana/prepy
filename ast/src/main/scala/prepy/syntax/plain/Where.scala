package prepy.syntax.plain

object Where {
  import prepy.syntax.plain.operators.getExpression
  import scala.reflect.macros.blackbox
  import scala.language.experimental.macros

  def stringifyPredicate[T <: Product: c.WeakTypeTag](
    c:         blackbox.Context
  )(predicate: c.Expr[T => Boolean]): c.Expr[String] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val serializedPredicate: c.Expr[String] = predicate.tree match {
      case q"(..$p) => $body" => c.Expr[String](q"${getExpression[T](body)(c).stringify}")
    }

    serializedPredicate
  }

  def impl[T <: Product: c.WeakTypeTag](
    c:         blackbox.Context
  )(predicate: c.Expr[T => Boolean]): c.Expr[`whereT`[T]] = {
    import c.universe._

    reify{
      `whereT`[T](c.prefix.splice.asInstanceOf[Query].toString, stringifyPredicate(c)(predicate).splice)
    }
  }

  case class `whereT`[T <: Product](queryElement: String, condition: String) extends Query {
    type Out = T

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
}
