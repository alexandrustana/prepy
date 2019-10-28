package prepy.syntax.ast.internal

import cats.data.Validated.Invalid
import shapeless.Typeable
import prepy.implicits.Implicits.Domain

private[prepy] trait Insert {

  def insert[T <: Product](implicit typeable: Typeable[T]): Insert.`insertT` =
    Insert.`insertT`(typeable.describe)

}

object Insert {
  private[syntax] case class `insertT`(tableName: String) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values`")

    def values[T <: Product](implicit inst: Domain[T]): `valuesT` = `valuesT`(this, inst.fields)

    override def toString: String = s"INSERT INTO $tableName"
  }

  private[syntax] case class `valuesT`(queryElement: Query, fields: List[Symbol]) extends Query {
    override def toString: String =
      s"$queryElement ${fields.map(_.name).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
