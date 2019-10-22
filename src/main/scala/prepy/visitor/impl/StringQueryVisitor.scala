package prepy.visitor.impl

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.Valid
import prepy.syntax.elements.QueryElement
import prepy.visitor.QueryVisitor

object StringQueryVisitor extends QueryVisitor {
  override type Out = String

  override def apply(elem: QueryElement): Validated[String, Out] = Valid(elem.toString)
}
