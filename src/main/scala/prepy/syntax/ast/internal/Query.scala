package prepy.syntax.ast.internal

import cats.data.Validated
import cats.data.Validated.Valid

private[syntax] trait Query { self =>

  def apply(): Validated[String, String] = Valid(self.toString)
}
