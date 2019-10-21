package prepy.syntax.elements

import prepy.visitor.QueryVisitor
import prepy.visitor.impl.StringQueryVisitor

private[prepy] trait QueryElement { self =>
  def apply(implicit visitor: QueryVisitor = StringQueryVisitor): visitor.Out = visitor.apply(self)
}
