package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.formatter.{Formatter, IdentityFormatter}
import prepy.implicits.Implicits._
import shapeless.ops.hlist.IsHCons
import shapeless.ops.coproduct.Selector
import shapeless.{:+:, CNil, Coproduct, HList, HNil, Poly1, Typeable}

private[syntax] trait Select {

  def select[T <: Product](implicit inst: Serialize[T], formatter: Formatter = IdentityFormatter): Select.`selectT`[T] =
    Select.`selectT`[T](inst.fields, formatter)

}

object Select extends Where {
  private[syntax] case class `selectT`[O <: Product](fields: List[Symbol], formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[T <: Product](implicit typeable: Typeable[T], transform: Transform[T, O]): Select.`fromT`[O, T] =
      `fromT`[O, T](this, typeable.describe, formatter)

    override def toString: String = s"SELECT ${fields.map(_.name).map(formatter.apply).mkString(", ")}"
  }

  private[syntax] case class `fromT`[O <: Product, T <: Product](
    elem:      Query,
    table:     String,
    formatter: Formatter
  ) extends Query {
    type Out = O

    def where(condition: T => Boolean): `whereT`[O, T :+: CNil] = `whereT`[O, T :+: CNil](this, "WHERE function T => Boolean")

    def join[K <: Product](implicit typeable: Typeable[K]): `joinT`[O, K :+: T :+: CNil] =
      `joinT`[O, K :+: T :+: CNil](elem, typeable.describe, formatter)

    override def toString: String = s"$elem FROM ${formatter(table)}"
  }

  private[syntax] case class `joinT`[O, T <: Coproduct](elem: Query, table: String, formatter: Formatter)
      extends Query {
    type Out = O

    override def apply() =
      Invalid("Incomplete SQL query. `join[T]` must be followed by a `on[S](S => Boolean)`")

    def on[S <: Product](condition: S => Boolean)(implicit selector: Selector[T, S]): `onT`[Out, T] =
      `onT`[Out, T](elem, condition, formatter)

    override def toString: String = s"$elem INNER JOIN ${formatter(table)}"
  }

  private[syntax] case class `onT`[O, T <: Coproduct](elem: Query, f: (_ <: Product) => Boolean, formatter: Formatter)
      extends Query {
    type Out = O

    def where[S <: Product](condition: S => Boolean)(implicit selector: Selector[T, S]): `whereT`[O, T] =
      `whereT`[O, T](this, "WHERE function T => Boolean")

    def join[K <: Product](implicit typeable: Typeable[K]): `joinT`[O, K :+: T] =
      `joinT`[O, K :+: T](elem, typeable.describe, formatter)

    override def toString: String = s"$elem ON function S => Boolean"
  }
}
