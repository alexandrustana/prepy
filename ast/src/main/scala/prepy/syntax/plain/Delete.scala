package prepy.syntax.plain

import prepy.syntax.implicits.Internal.IdentityTransform
import prepy.syntax.internal.Codec

private[syntax] trait Delete {

  def delete[T <: Product](
    implicit transform: IdentityTransform[T]
  ): Delete.`deleteT`[T] =
    Delete.`deleteT`[T](transform.from.name)
}

object Delete {
  private[syntax] case class `deleteT`[T <: Product](tableName: String) extends Query {
    import Where._

    import scala.language.experimental.macros

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"DELETE FROM ${Codec.encode(tableName)}"
  }

}
