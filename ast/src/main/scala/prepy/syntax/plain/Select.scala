package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec

private[syntax] trait Select {

  def select[T <: Product](implicit inst: IdentityTransform[T]): Select.`selectT`[T] =
    Select.`selectT`[T](inst.from.fields)

}

object Select {
  private[syntax] case class `selectT`[T <: Product](fields: List[Symbol]) extends Query {
    override def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K <: Product](implicit transform: Transform[K, T]): Select.`fromT`[T] =
      `fromT`[T](this, transform.from.name)

    override def toString: String = s"SELECT ${fields.map(_.name).map(Codec.encode).mkString(", ")}"
  }

  private[syntax] case class `fromT`[T <: Product](queryElement: Query, tableName: String) extends Query {
    import Where._

    import scala.language.experimental.macros

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"$queryElement FROM ${Codec.encode(tableName)}"
  }
}
