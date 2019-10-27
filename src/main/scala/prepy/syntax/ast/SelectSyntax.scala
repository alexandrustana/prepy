package prepy.syntax.ast

import cats.data.Validated.Invalid
import prepy.Domain
import prepy.syntax.factory.SelectFactory
import shapeless.Typeable

private[prepy] trait SelectSyntax {

  def select[T <: Product](implicit inst: Domain[T], factory: SelectFactory): SelectSyntax.`selectT` =
    factory.`selectT`(inst.fields, factory)

}

object SelectSyntax {
  abstract private[syntax] class `selectT`(fields: List[Symbol], factory: SelectFactory) extends QueryElement {
    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K](implicit typeable: Typeable[K]): SelectSyntax.`fromT` =
      factory.`fromT`(this, typeable.describe, factory)

    override def toString: String = s"SELECT ${fields.map(_.name).mkString(", ")}"
  }

  abstract private[syntax] class `fromT`(
    queryElement: QueryElement,
    tableName:    String,
    factory:      SelectFactory
  ) extends QueryElement {
    def where(condition: String): `whereT` = factory.`whereT`(this, condition, factory)

    override def toString: String = s"$queryElement FROM $tableName"
  }

  abstract private[syntax] class logicalOp(queryElement: QueryElement, condition: String, factory: SelectFactory)
      extends QueryElement {
    def and(condition: String): `andT` = factory.`andT`(this, condition, factory)
    def or(condition:  String): `orT`  = factory.`orT`(this, condition, factory)
  }
  abstract private[syntax] class `whereT`(queryElement: QueryElement, condition: String, factory: SelectFactory)
      extends logicalOp(queryElement, condition, factory) {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  abstract private[syntax] class `andT`(queryElement: QueryElement, condition: String, factory: SelectFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  abstract private[syntax] class `orT`(queryElement: QueryElement, condition: String, factory: SelectFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
