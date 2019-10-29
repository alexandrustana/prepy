package prepy.syntax.ast.internal

import prepy.formatter.{Formatter, IdentityFormatter}
import shapeless.Typeable

private[syntax] trait Delete {

  def delete[T <: Product](implicit typeable: Typeable[T], formatter: Formatter = IdentityFormatter): Delete.`deleteT` =
    Delete.`deleteT`(typeable.describe, formatter)
}

object Delete extends Where {
  private[syntax] case class `deleteT`(tableName: String, formatter: Formatter) extends Query with Delete {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String = s"DELETE FROM ${formatter(tableName)}"
  }

}
