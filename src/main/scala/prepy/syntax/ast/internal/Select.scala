package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.formatter.{Formatter, IdentityFormatter}
import prepy.implicits.Implicits._
import shapeless.ops.hlist.IsHCons
import shapeless.{:+:, CNil, Coproduct, HList, HNil, Typeable}

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

    def where(f: T => Boolean): `whereT`[O, T :+: CNil] = `whereT`[O, T :+: CNil](this, "WHERE function T => Boolean")

    def join[K <: Product](implicit typeable: Typeable[K]): `joinT`[O, K :+: T :+: CNil] =
      `joinT`[O, K :+: T :+: CNil](elem, typeable.describe, formatter)

    override def toString: String = s"$elem FROM ${formatter(table)}"
  }

  private[syntax] case class `joinT`[O, T <: Coproduct](elem: Query, table: String, formatter: Formatter)
      extends Query {
    type Out = O

    def on[S <: Product](f: S => Option[Boolean]): `onT`[Out, T] = `onT`[Out, T](elem, , formatter)

    override def toString: String = s"$elem INNER JOIN ${formatter(table)}"
  }

  private[syntax] case class `onT`[O, T <: Coproduct](elem: Query, f: T => Option[Boolean], formatter: Formatter)
      extends Query {
    type Out = O

    def where(f: T => Option[Boolean]): `whereT`[O, T] =
      `whereT`[O, T](this, "WHERE function (T*) => Boolean")

    def join[K <: Product](implicit typeable: Typeable[K]): `joinT`[O, K :+: T] =
      `joinT`[O, K :+: T](elem, typeable.describe, formatter)
  }
}
