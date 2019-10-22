package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Valid

private[prepy] trait QueryElement { self =>

  def apply(): Validated[String, String] = Valid(self.toString)
}
