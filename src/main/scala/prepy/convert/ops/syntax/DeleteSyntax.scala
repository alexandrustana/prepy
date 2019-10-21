package prepy.convert.ops.syntax

import prepy.convert.ops.syntax.inner.{QueryElement, WhereSyntax}
import shapeless.Typeable

trait DeleteSyntax extends WhereSyntax {

  def delete[T <: Product](implicit typeable: Typeable[T]): `deleteT` =
    `deleteT`(typeable.describe)

  private[DeleteSyntax] case class `deleteT`(tableName: String) extends QueryElement {
    def apply(): String = toString

    def where(condition: String*): `whereT` = `whereT`(this, condition.toList)

    override def toString: String = s"DELETE FROM $tableName"
  }
}
