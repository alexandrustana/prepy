package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.formatter.Formatter
import prepy.formatter.identity.IdentityFormatter
import prepy.syntax.internal.Codec

trait Query { self =>

  def apply()(implicit formatter: Formatter = IdentityFormatter): Validated[String, String] =
    Valid(self.toString)
      .map(
        _.split(" ")
          .map(word => if (Codec.isEncoded(word)) formatter.apply(Codec.decode(word)) else word)
          .mkString(" ")
      )
}
