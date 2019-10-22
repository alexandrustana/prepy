package prepy.syntax.plain

import shapeless.Typeable

private[prepy] trait DeleteSyntax extends WhereSyntax {

  def delete[T <: Product](implicit typeable: Typeable[T]): `deleteT` =
    `deleteT`(typeable.describe)

  private[prepy] case class `deleteT`(tableName: String) extends QueryElement {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String = s"DELETE FROM $tableName"
  }
}
