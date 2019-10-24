package prepy.syntax.plain

import prepy.syntax.factory.DeleteFactory
import shapeless.Typeable

private[prepy] trait DeleteSyntax {

  def delete[T <: Product](implicit typeable: Typeable[T], factory: DeleteFactory): DeleteSyntax.`deleteT` =
    factory.`deleteT`(typeable.describe, factory)
}

object DeleteSyntax {
  abstract private[syntax] class `deleteT`(tableName: String, factory: DeleteFactory) extends QueryElement {
    def where(condition: String): `whereT` = factory.`whereT`(this, condition, factory)

    override def toString: String = s"DELETE FROM $tableName"
  }

  abstract private[syntax] class logicalOp(queryElement: QueryElement, condition: String, factory: DeleteFactory)
      extends QueryElement {
    def and(condition: String): `andT` = factory.`andT`(this, condition, factory)
    def or(condition:  String): `orT`  = factory.`orT`(this, condition, factory)
  }
  abstract private[syntax] class `whereT`(queryElement: QueryElement, condition: String, factory: DeleteFactory)
      extends logicalOp(queryElement, condition, factory) {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  abstract private[syntax] class `andT`(queryElement: QueryElement, condition: String, factory: DeleteFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  abstract private[syntax] class `orT`(queryElement: QueryElement, condition: String, factory: DeleteFactory)
      extends logicalOp(queryElement, condition, factory) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
