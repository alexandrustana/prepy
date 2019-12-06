package prepy.syntax.plain

import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Implicits._
import shapeless.Typeable

private[syntax] trait Update {

  def update[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Update.`updateT`[T] =
    Update.`updateT`[T](typeable.describe, formatter)

}

object Update {

  private[syntax] case class `updateT`[T <: Product](tableName: String, formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")

    def set[K <: Product](implicit domain: Serialize[K], transform: Transform[T, K]): `setT`[K] =
      `setT`[K](this, domain.fields, formatter)

    override def toString: String = s"UPDATE ${formatter(tableName)}"
  }

  private[syntax] case class `setT`[T <: Product](queryElement: Query, fields: List[Symbol], formatter: Formatter)
      extends Query {
    import scala.language.experimental.macros
    import Where._

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${formatter(field.name)} = ?").mkString(", ")}"
  }
}
