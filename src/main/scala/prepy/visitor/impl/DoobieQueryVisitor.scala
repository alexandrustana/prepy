package prepy.visitor.impl

import doobie._
import doobie.implicits._
import doobie.util.fragment
import prepy.syntax.elements.{QueryElement, SelectSyntax}
import prepy.visitor.QueryVisitor

object DoobieQueryVisitor extends QueryVisitor {
  override type Out = Fragment

  override def apply(elem: QueryElement) = ???
}
