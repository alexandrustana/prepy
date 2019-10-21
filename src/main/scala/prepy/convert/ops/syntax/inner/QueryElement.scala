package prepy.convert.ops.syntax.inner

import prepy.convert.ops.syntax.inner.visitor.QueryVisitor
import prepy.convert.ops.syntax.inner.visitor.impl.StringQueryVisitor

trait QueryElement { self =>
  def apply(implicit visitor: QueryVisitor = StringQueryVisitor): visitor.Out = visitor.apply(self)
}
