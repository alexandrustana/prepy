package prepy.syntax.query

import cats.effect.Sync
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.internal.Codec

trait Query { self =>

  def apply[F[_]: Sync]()(implicit formatter: Formatter = IdentityFormatter, F: Sync[F]) =
    F.pure {
      self.toString
        .split(" ")
        .map(word => if (Codec.isEncoded(word)) formatter.apply(Codec.decode(word)) else word)
        .mkString(" ")
    }
}
