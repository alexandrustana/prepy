package prepy.syntax.ast

import cats.data.Validated
import cats.data.Validated.Valid

private[prepy] trait QueryElement { self =>

  def apply(): Validated[String, _]
}
