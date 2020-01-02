package prepy.syntax.plain

import cats.data.Validated
import cats.data.Validated.Valid

trait Query { self =>

  def apply(): Validated[String, String] = Valid(self.toString)
}
