package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import prepy.implicits.Implicits.Domain
import shapeless.Typeable

private[syntax] trait Select {

  def select[T <: Product](implicit inst: Domain[T]): Select.`selectT` =
    Select.`selectT`(inst.fields)

}

object Select extends Where {
  private[syntax] case class `selectT`(fields: List[Symbol]) extends Query with Select {
    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K](implicit typeable: Typeable[K]): Select.`fromT` =
      `fromT`(this, typeable.describe)

    override def toString: String = s"SELECT ${fields.map(_.name).mkString(", ")}"
  }

  private[syntax] case class `fromT`(
    queryElement: Query,
    tableName:    String
  ) extends Query with Select {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String = s"$queryElement FROM $tableName"
  }

}
