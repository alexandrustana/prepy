package prepy.syntax.elements

import prepy.Domain
import shapeless.Typeable

private[prepy] trait SelectSyntax extends WhereSyntax {

  def select[T <: Product](implicit inst: Domain[T]): `selectT` = `selectT`(inst.fields)

  private[prepy] case class `selectT`(fields: List[Symbol]) extends QueryElement {
    def from[T](implicit typeable: Typeable[T]): `fromT` = `fromT`(this, typeable.describe)

    override def toString: String = s"SELECT ${fields.map(_.name).mkString(",")}"
  }

  private[prepy] case class `fromT`(queryElement: QueryElement, tableName: String) extends QueryElement {
    def where(conditions: String*): `whereT` = `whereT`(this, conditions.toList)

    override def toString: String = s"$queryElement FROM $tableName"
  }

}
