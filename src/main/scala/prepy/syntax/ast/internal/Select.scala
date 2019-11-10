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
  private[syntax] case class `selectT`[T <: Product](fields: List[Symbol], formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K <: Product](implicit typeable: Typeable[K], transform: Transform[K, T]): Select.`fromT`[T] =
      `fromT`[T, K](this, typeable.describe, formatter)

    override def toString: String = s"SELECT ${fields.map(_.name).map(formatter.apply).mkString(", ")}"
  }

  private[syntax] case class `fromT`[O <: Product, T <: Product](
    elem:      Query,
    table:     String,
    formatter: Formatter
  ) extends Query {
    type Out = O

    def where(condition: String): `whereT`[O, T] = `whereT`[O, T](this, condition)

    def join[K <: Product]: `joinT`[Out, T :+: K :+: CNil] =
      `joinT`[O, T :+: K :+: CNil](elem, formatter)

    override def toString: String = s"$elem FROM ${formatter(table)}"
  }

  private[syntax] case class `joinT`[O, T <: Coproduct](elem: Query, formatter: Formatter) extends Query {
    type Out = O

    def on(f: T => Boolean): `onT`[Out, T] = `onT`[Out, T](elem, f, formatter)
  }

  private[syntax] case class `onT`[O, T <: Coproduct](elem: Query, f: T => Boolean, formatter: Formatter)
      extends Query {
    type Out = O

    def where(condition: String): `whereT`[O, T] = `whereT`[O, T](this, condition)

    def join[K <: Product]: `joinT`[O, K :+: T] =
      `joinT`[O, K :+: T](elem, formatter)
  }
}
