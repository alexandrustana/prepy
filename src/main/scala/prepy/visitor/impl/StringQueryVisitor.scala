package prepy.visitor.impl

import prepy.syntax.elements.QueryElement
import prepy.visitor.QueryVisitor

object StringQueryVisitor extends QueryVisitor{
  override type Out = String

  override def apply(elem: QueryElement): Out = elem.toString
}
