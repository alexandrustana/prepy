package prepy.syntax.plain

import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Implicits._
import shapeless.Typeable

private[prepy] trait Insert {

  def insert[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Insert.`insertT`[T] =
    Insert.`insertT`[T](typeable.describe, formatter)

}

object Insert {
  private[syntax] case class `insertT`[T <: Product](tableName: String, formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values[K]`")

    def values[K <: Product](implicit inst: Serialize[K], transform: Transform[T, K]): `valuesT`[K] =
      `valuesT`[K](this, inst.fields, formatter)

    override def toString: String = s"INSERT INTO ${formatter(tableName)}"
  }

  private[syntax] case class `valuesT`[T <: Product](queryElement: Query, fields: List[Symbol], formatter: Formatter)
      extends Query {
    type Out = T

    override def toString: String =
      s"$queryElement ${fields.map(_.name).map(formatter.apply).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
