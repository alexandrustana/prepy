package prepy.convert.ops

import prepy.convert.Domain
import shapeless.Typeable

object DeleteSyntax {

  def select[T <: Product](implicit inst: Domain[T]): SelectElem = SelectElem(inst.fields)

  case class SelectElem(fields: List[Symbol]) {
    def from[T](implicit typeable: Typeable[T]) = WhereElem(fields, typeable.describe)
  }

  case class WhereElem(fields: List[Symbol], tableName: String) {

    def apply(): String = s"SELECT ${fields.map(_.name).mkString(",")} FROM $tableName"

    def where(condition: String*): String =
      s"SELECT ${fields.map(_.name).mkString(",")} FROM $tableName WHERE ${condition.mkString("(", "AND", ")")}"
  }
}
