package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec
import shapeless.Typeable

private[prepy] trait Insert {

  def insert[T <: Product](
    implicit typeable: Typeable[T]
  ): Insert.`insertT`[T] =
    Insert.`insertT`[T](typeable.describe)

}

object Insert {
  private[syntax] case class `insertT`[T <: Product](tableName: String) extends Query {
    override def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values[K]`")

    def values[K <: Product](implicit inst: Serialize[K], transform: Transform[T, K]): `valuesT`[K] =
      `valuesT`[K](this, inst.fields)

    override def toString: String = s"INSERT INTO ${Codec.encode(tableName)}"
  }

  private[syntax] case class `valuesT`[T <: Product](queryElement: Query, fields: List[Symbol]) extends Query {
    type Out = T

    override def toString: String =
      s"$queryElement ${fields.map(_.name).map(Codec.encode).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
