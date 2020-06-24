package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec

private[syntax] trait Update {

  def update[T <: Product](
    implicit transform: IdentityTransform[T]
  ): Update.`updateT`[T] =
    Update.`updateT`[T](transform.to.name)

}

object Update {

  private[syntax] case class `updateT`[T <: Product](tableName: String) extends Query {
    override def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")

    def set[K <: Product](implicit transform: Transform[T, K]): `setT`[K] =
      `setT`[K](this, transform.to.fields)

    override def toString: String = s"UPDATE ${Codec.encode(tableName)}"
  }

  private[syntax] case class `setT`[T <: Product](queryElement: Query, fields: List[Symbol]) extends Query {
    import Where._

    import scala.language.experimental.macros

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${Codec.encode(field.name)} = ?").mkString(", ")}"
  }
}
