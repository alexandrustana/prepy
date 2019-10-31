package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.formatter.{Formatter, IdentityFormatter}
import prepy.implicits.Implicits.Domain
import shapeless.Typeable

private[syntax] trait Update {

  def update[T <: Product](
    implicit typeable: Typeable[T],
    formatter:         Formatter = IdentityFormatter
  ): Update.`updateT` =
    Update.`updateT`(typeable.describe, formatter)

}

object Update extends Where {

  private[syntax] case class `updateT`(tableName: String, formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")

    def set[K <: Product](implicit domain: Domain[K]): `setT`[K] = `setT`[K](this, domain.fields, formatter)

    override def toString: String = s"UPDATE ${formatter(tableName)}"
  }

  private[syntax] case class `setT`[T <: Product](queryElement: Query, fields: List[Symbol], formatter: Formatter)
      extends Query {
    type Out = T

    def where(condition: String): `whereT`[T] = `whereT`[T](this, condition)

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${formatter(field.name)} = ?").mkString(", ")}"
  }
}
