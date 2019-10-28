package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.implicits.Implicits.Domain
import shapeless.Typeable

private[prepy] trait Update {

  def update[T <: Product](implicit typeable: Typeable[T]): Update.`updateT` =
    Update.`updateT`(typeable.describe)

}

object Update extends Where {

  private[syntax] case class `updateT`(tableName: String) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[T]`")

    def set[T <: Product](implicit domain: Domain[T]): `setT` = `setT`(this, domain.fields)

    override def toString: String = s"UPDATE $tableName"
  }

  private[syntax] case class `setT`(queryElement: Query, fields: List[Symbol]) extends Query {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String =
      s"$queryElement SET ${fields.map(field => s"${field.name} = ?").mkString(", ")}"
  }
}
