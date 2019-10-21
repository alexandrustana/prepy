package prepy.convert.ops.syntax.inner.visitor

import prepy.convert.ops.syntax.inner.QueryElement

trait QueryVisitor {
  type Out

  def apply(elem: QueryElement): Out
}
