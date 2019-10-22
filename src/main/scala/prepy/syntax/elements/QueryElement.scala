package prepy.syntax.elements

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.interpreter.StringQueryInterpreter

private[prepy] trait QueryElement { self =>

  def apply(): Validated[String, String] = Valid(self.toString)
}
