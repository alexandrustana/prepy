package prepy.syntax.query

import cats.effect.Sync
import prepy.formatter.Formatter
import prepy.syntax.implicits.Internal._
import prepy.syntax.internal.Codec
import prepy.syntax.query.expection.InvalidQuery

private[prepy] trait Insert {

  def insert[T <: Product](
    implicit transform: IdentityTransform[T]
  ): Insert.`insertT`[T] =
    Insert.`insertT`[T](transform.to.name)

}

object Insert {
  private[syntax] case class `insertT`[T <: Product](tableName: String) extends Query {
    override def apply[F[_]: Sync]()(implicit formatter: Formatter, F: Sync[F]): F[String] =
      F.raiseError(InvalidQuery("Incomplete SQL query. `insert[T]` must be followed by a `values[K]`"))

    def values[K <: Product](implicit transform: Transform[T, K]): `valuesT`[K] =
      `valuesT`[K](this, transform.to.fields)

    override def toString: String = s"INSERT INTO ${Codec.encode(tableName)}"
  }

  private[syntax] case class `valuesT`[T <: Product](queryElement: Query, fields: List[Symbol]) extends Query {
    type Out = T

    override def toString: String =
      s"$queryElement ${fields.map(_.name).map(Codec.encode).mkString("(", ", ", ")")} VALUES ${fields.map(_ => "?").mkString("(", ", ", ")")}"
  }
}
