package prepy.syntax.plain

import cats.data.Validated.Invalid
import prepy.Domain
import shapeless.Typeable

private[prepy] trait SelectSyntax extends WhereSyntax {

  def select[T <: Product](implicit inst: Domain[T]): `selectT` = `selectT`(inst.fields)

  def select(): `selectT` = `selectT`(List(Symbol("*")))

  private[syntax] case class `selectT`(fields: List[Symbol]) extends QueryElement {

    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[T]`")

    def from[T](implicit typeable: Typeable[T]): `fromT` = `fromT`(this, typeable.describe)

    override def toString: String = s"SELECT ${fields.map(_.name).mkString(", ")}"
  }

  private[syntax] case class `fromT`(queryElement: QueryElement, tableName: String) extends QueryElement {
    def where(condition: String): `whereT` = `whereT`(this, condition)

    override def toString: String = s"$queryElement FROM $tableName"
  }

}
