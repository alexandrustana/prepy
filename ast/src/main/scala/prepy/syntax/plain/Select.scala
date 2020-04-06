package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Invalid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec
import shapeless.ops.coproduct._
import shapeless._

private[syntax] trait Select {

  def select[Output <: Product](implicit inst: Serialize[Output]): Select.`selectT`[Output] =
    Select.`selectT`[Output](inst.fields)

}

object Select {
  private[syntax] case class `selectT`[Output <: Product](fields: List[Symbol]) extends Query {
    override def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
      Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")

    def from[K <: Product](implicit typeable: Typeable[K], transform: Transform[K, Output]): Select.`fromT`[Output] =
      `fromT`[Output](this, typeable.describe)

    override def toString: String = s"SELECT ${fields.map(_.name).map(Codec.encode).mkString(", ")}"
  }

  private[syntax] case class `fromT`[Output <: Product](queryElement: Query, tableName: String) extends Query {
    import scala.language.experimental.macros
    import Where._
    import Join._

    type Cons[T] = T :+: Output :+: CNil

    def join[Table <: Product](implicit typeable: Typeable[Table]): `joinT`[Table, Cons[Table]] =
      `joinT`[Table, Cons[Table]](this, typeable.describe)

    def where(predicate: Output => Boolean): `whereT`[Output] = macro Where.impl[Output]

    override def toString: String = s"$queryElement FROM ${Codec.encode(tableName)}"
  }

}
