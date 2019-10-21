package prepy.convert.ops.syntax

import prepy.convert.Domain
import prepy.convert.ops.syntax.inner.{QueryElement, WhereSyntax}
import shapeless.Typeable

trait SelectSyntax extends WhereSyntax {

  def select[T <: Product](implicit inst: Domain[T]): `selectT` = `selectT`(inst.fields)

  private[SelectSyntax] case class `selectT`(fields: List[Symbol]) extends QueryElement {
    def from[T](implicit typeable: Typeable[T]): `fromT` = `fromT`(this, typeable.describe)

    override def toString: String = s"SELECT ${fields.map(_.name).mkString(",")}"
  }

  private[SelectSyntax] case class `fromT`(queryElement: QueryElement, tableName: String) extends QueryElement {
    def apply(): String = toString

    def where(conditions: String*): `whereT` = `whereT`(this, conditions.toList)

    override def toString: String = s"$queryElement FROM $tableName"
  }

}
