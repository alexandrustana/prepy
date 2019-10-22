package prepy.visitor

import prepy.syntax.elements.QueryElement

trait QueryVisitor {
  type Out

  def apply(elem: QueryElement): QueryVisitor#Out
}
