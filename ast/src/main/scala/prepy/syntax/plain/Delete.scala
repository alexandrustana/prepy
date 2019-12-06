package prepy.syntax.plain

import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import shapeless.Typeable

private[syntax] trait Delete {

  def delete[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Delete.`deleteT`[T] =
    Delete.`deleteT`[T](typeable.describe, formatter)
}

object Delete {
  private[syntax] case class `deleteT`[T <: Product](tableName: String, formatter: Formatter) extends Query {
    import scala.language.experimental.macros
    import Where._

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"DELETE FROM ${formatter(tableName)}"
  }

}
