package prepy.syntax.ast

import cats.data.Validated.Invalid
import prepy.Domain
import prepy.syntax.factory.UpdateFactory
import shapeless.Typeable

private[prepy] trait UpdateSyntax {

  def update[T <: Product](implicit typeable: Typeable[T], factory: UpdateFactory): UpdateSyntax.`updateT` =
    factory.`updateT`(typeable.describe, factory)

}

object UpdateSyntax {

  abstract private[syntax] class `updateT`(tableName: String, factory: UpdateFactory) extends QueryElement {
    override def apply() =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[T]`")

    def set[T](implicit domain: Domain[T]): `setT` = factory.`setT`(this, domain.fields, factory)

    override def toString: String = s"UPDATE $tableName"
  }

  abstract private[syntax] class `setT`(queryElement: QueryElement, fields: List[Symbol], factory: UpdateFactory)
      extends QueryElement {
    def where(condition: String): `whereT` = factory.`whereT`(this, condition, factory)

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${field.name} = ?").mkString(", ")}"
  }

  abstract private[syntax] class logicalOp(queryElement: QueryElement, condition: String, factory: UpdateFactory)
      extends QueryElement {
    def and(condition: String): `andT` = factory.`andT`(this, condition, factory)
    def or(condition:  String): `orT`  = factory.`orT`(this, condition, factory)
  }
  abstract private[syntax] class `whereT`(queryElement: QueryElement, condition: String, factory: UpdateFactory)
      extends logicalOp(queryElement, condition, factory) {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  abstract private[syntax] class `andT`(queryElement: QueryElement, condition: String, factory: UpdateFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  abstract private[syntax] class `orT`(queryElement: QueryElement, condition: String, factory: UpdateFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
