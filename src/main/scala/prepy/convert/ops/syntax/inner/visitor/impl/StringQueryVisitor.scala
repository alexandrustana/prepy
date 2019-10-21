package prepy.convert.ops.syntax.inner.visitor.impl

import prepy.convert.ops.syntax.inner.{QueryElement}
import prepy.convert.ops.syntax.inner.visitor.QueryVisitor

object StringQueryVisitor extends QueryVisitor{
  override type Out = String

  override def apply(elem: QueryElement): String = elem.toString
}
