package prepy.syntax.elements

import cats.data.Validated.Invalid
import prepy.Domain
import shapeless.Typeable

private[prepy] trait UpdateSyntax extends WhereSyntax {

  def update[T <: Product](implicit typeable: Typeable[T]): `updateT` =
    `updateT`(typeable.describe)

  private[prepy] case class `updateT`(tableName: String) extends QueryElement {
    override def apply() =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[T]`")

    def set[T](implicit domain: Domain[T]): `setT` = `setT`(this, domain.fields)

    override def toString: String = s"UPDATE $tableName"
  }

  private[prepy] case class `setT`(queryElement: QueryElement, fields: List[Symbol]) extends QueryElement {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${field.name} = ?").mkString(", ")}"
  }
}
