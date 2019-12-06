package prepy.syntax.plain

import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Implicits._
import shapeless.Typeable

private[syntax] trait Select {

  def select[T <: Product](implicit inst: Serialize[T], formatter: Formatter = IdentityFormatter): Select.`selectT`[T] =
    Select.`selectT`[T](inst.fields, formatter)

}

object Select {
  private[syntax] case class `selectT`[T <: Product](fields: List[Symbol], formatter: Formatter) extends Query {
    override def apply() =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K <: Product](implicit typeable: Typeable[K], transform: Transform[K, T]): Select.`fromT`[T] =
      `fromT`[T](this, typeable.describe, formatter)

    override def toString: String = s"SELECT ${fields.map(_.name).map(formatter.apply).mkString(", ")}"
  }

  private[syntax] case class `fromT`[T <: Product](
    queryElement: Query,
    tableName:    String,
    formatter:    Formatter
  ) extends Query {
    import scala.language.experimental.macros
    import Where._

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"$queryElement FROM ${formatter(tableName)}"
  }
}
