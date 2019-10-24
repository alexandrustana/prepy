package prepy.syntax.ast

import cats.data.Validated.Invalid
import prepy.Domain
import prepy.syntax.factory.InsertFactory
import shapeless.Typeable

private[prepy] trait InsertSyntax {

  def insert[T <: Product](implicit typeable: Typeable[T], factory: InsertFactory): InsertSyntax.`insertT` =
    factory.`insertT`(typeable.describe, factory)

}

object InsertSyntax {
  abstract private[syntax] class `insertT`(tableName: String, factory: InsertFactory) extends QueryElement {
    override def apply() =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values`")

    def values[T <: Product](implicit inst: Domain[T]): `valuesT` = factory.`valuesT`(this, inst.fields, factory)

    override def toString: String = s"INSERT INTO $tableName"
  }

  abstract private[syntax] class `valuesT`(queryElement: QueryElement, fields: List[Symbol], factory: InsertFactory)
      extends QueryElement {
    override def toString: String =
      s"$queryElement ${fields.map(_.name).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
