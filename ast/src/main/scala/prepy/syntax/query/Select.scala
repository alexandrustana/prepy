package prepy.syntax.query

import cats.effect.Sync
import prepy.formatter.Formatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec
import prepy.syntax.query.expection.InvalidQuery

private[syntax] trait Select {

  def select[T <: Product](implicit inst: IdentityTransform[T]): Select.`selectT`[T] =
    Select.`selectT`[T](inst.from.fields)

}

object Select {
  private[syntax] case class `selectT`[T <: Product](fields: List[Symbol]) extends Query {
    override def apply[F[_]: Sync]()(implicit formatter: Formatter, F: Sync[F]): F[String] =
      F.raiseError(InvalidQuery("Incomplete SQL query. `select[T]` must be followed by a `from[K]`"))

    def from[K <: Product](implicit transform: Transform[K, T]): Select.`fromT`[T] =
      `fromT`[T](this, transform.from.name)

    override def toString: String = s"SELECT ${fields.map(_.name).map(Codec.encode).mkString(", ")}"
  }

  private[syntax] case class `fromT`[T <: Product](queryElement: Query, tableName: String) extends Query {
    import Where._

    import scala.language.experimental.macros

    type Out = T

    def where(predicate: T => Boolean): `whereT`[T] = macro impl[T]

    override def toString: String = s"$queryElement FROM ${Codec.encode(tableName)}"
  }
}
