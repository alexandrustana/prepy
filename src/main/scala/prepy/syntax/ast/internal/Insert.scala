package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.formatter.{Formatter, IdentityFormatter}
import shapeless.Typeable
import prepy.implicits.Implicits.Domain

private[prepy] trait Insert {

  def insert[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Insert.`insertT` =
    Insert.`insertT`(typeable.describe, formatter)

}

object Insert {
  private[syntax] case class `insertT`(tableName: String, formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values[K]`")

    def values[K <: Product](implicit inst: Domain[K]): `valuesT`[K] = `valuesT`[K](this, inst.fields, formatter)

    override def toString: String = s"INSERT INTO ${formatter(tableName)}"
  }

  private[syntax] case class `valuesT`[T <: Product](queryElement: Query, fields: List[Symbol], formatter: Formatter)
      extends Query {
    type Out = T

    override def toString: String =
      s"$queryElement ${fields.map(_.name).map(formatter.apply).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
