package prepy.syntax.ast.internal

import shapeless.Typeable

private[syntax] trait Delete {

  def delete[T <: Product](implicit typeable: Typeable[T]): Delete.`deleteT` =
    Delete.`deleteT`(typeable.describe)
}

object Delete extends Where {
  private[syntax] case class `deleteT`(tableName: String) extends Query with Delete {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String = s"DELETE FROM $tableName"
  }

}
