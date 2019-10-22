package prepy.syntax.elements

import cats.data.Validated.Invalid
import prepy.Domain
import prepy.visitor.QueryVisitor
import shapeless.Typeable

private[prepy] trait InsertSyntax {

  def insert[T <: Product](implicit typeable: Typeable[T], domain: Domain[T]): `insertT` =
    `insertT`(typeable.describe, domain.fields)

  private[prepy] case class `insertT`(tableName: String, fields: List[Symbol]) extends QueryElement {
    override def apply(implicit visitor: QueryVisitor) =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values`")

    def values(): `valuesT` = `valuesT`(this, fields)

    override def toString: String = s"INSERT INTO $tableName ${fields.map(_.name).mkString("(", ", ", ")")}"
  }

  private[prepy] case class `valuesT`(queryElement: QueryElement, fields: List[Symbol]) extends QueryElement {
    override def toString: String =
      s"$queryElement VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
