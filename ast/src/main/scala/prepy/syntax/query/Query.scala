package prepy.syntax.query

import cats.MonadError
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.internal.Codec

trait Query { self =>

  def apply[F[_]]()(implicit formatter: Formatter = IdentityFormatter, F: MonadError[F, Throwable]): F[String] =
    F.pure {
      self.toString
        .split(" ")
        .map(word => if (Codec.isEncoded(word)) formatter.apply(Codec.decode(word)) else word)
        .mkString(" ")
    }
}
