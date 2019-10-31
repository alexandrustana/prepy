package prepy.syntax.ast.internal

import prepy.formatter.{Formatter, IdentityFormatter}
import shapeless.Typeable

private[syntax] trait Delete {

  def delete[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Delete.`deleteT`[T] =
    Delete.`deleteT`[T](typeable.describe, formatter)
}

object Delete extends Where {
  private[syntax] case class `deleteT`[T <: Product](tableName: String, formatter: Formatter) extends Query {
    type Out = T

    def where(condition: String): `whereT`[T] = `whereT`[T](this, condition)

    override def toString: String = s"DELETE FROM ${formatter(tableName)}"
  }

}
