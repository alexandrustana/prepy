package prepy.syntax.plain

import cats.data.Validated.Invalid
import prepy.Domain
import shapeless.Typeable

private[prepy] trait InsertSyntax {

  def insert[T <: Product](implicit typeable: Typeable[T]): `insertT` =
    `insertT`(typeable.describe)

  private[prepy] case class `insertT`(tableName: String) extends QueryElement {
    override def apply() =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values`")

    def values[T <: Product](implicit inst: Domain[T]): `valuesT` = `valuesT`(this, inst.fields)

    override def toString: String = s"INSERT INTO $tableName"
  }

  private[prepy] case class `valuesT`(queryElement: QueryElement, fields: List[Symbol]) extends QueryElement {
    override def toString: String =
      s"$queryElement ${fields.map(_.name).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
