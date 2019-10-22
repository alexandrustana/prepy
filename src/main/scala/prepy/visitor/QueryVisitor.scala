package prepy.visitor

import cats.data.Validated
import prepy.syntax.elements.QueryElement

trait QueryVisitor {
  type Out

  def apply(elem: QueryElement): Validated[String, Out]
}
