package prepy.syntax.elements

import cats.data.Validated
import prepy.visitor.QueryVisitor
import prepy.visitor.impl.StringQueryVisitor

private[prepy] trait QueryElement { self =>

  def apply(implicit visitor: QueryVisitor = StringQueryVisitor): Validated[String, visitor.Out] =
    visitor.apply(self)
}
