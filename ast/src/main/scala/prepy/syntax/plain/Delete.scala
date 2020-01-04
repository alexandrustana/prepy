package prepy.syntax.plain

import prepy.syntax.internal.Codec
import shapeless.Typeable

private[syntax] trait Delete {

  def delete[T <: Product](
    implicit typeable: Typeable[T]
  ): Delete.`deleteT`[T] =
    Delete.`deleteT`[T](typeable.describe)
}

object Delete {
  private[syntax] case class `deleteT`[T <: Product](tableName: String) extends Query {
    import scala.language.experimental.macros
    import Where._

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"DELETE FROM ${Codec.encode(tableName)}"
  }

}
