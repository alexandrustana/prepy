package prepy.convert.ops.syntax.inner

import shapeless.Typeable

private[syntax] trait DeleteSyntax extends WhereSyntax {

  def delete[T <: Product](implicit typeable: Typeable[T]): `deleteT` =
    `deleteT`(typeable.describe)

  private[syntax] case class `deleteT`(tableName: String) extends QueryElement {
    def where(condition: String*): `whereT` = `whereT`(this, condition.toList)

    override def toString: String = s"DELETE FROM $tableName"
  }
}
