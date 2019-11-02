package prepy.syntax.doobie.internal

import doobie.implicits._
import doobie.util.fragment.Fragment
import doobie.util.fragments.parentheses
import prepy.syntax.ast.internal.{Query, Where => internal}

private[internal] trait Where extends internal {
  abstract private[internal] class logicalOpD[T <: Product](elem: Query, predicate: Fragment) extends logicalOp[T] {
    override def queryElement: Query  = elem
    override def condition:    String = predicate.toString
    override def toString:     String = elem.toString

    def compile(): Fragment

    def and(predicate: Fragment): `andD`[T] = `andD`[T](this, predicate)
    def or(predicate:  Fragment): `orD`[T]  = `orD`[T](this, predicate)
  }

  private[internal] case class `whereD`[T <: Product](elem: Query, predicate: Fragment)
      extends logicalOpD[T](elem, predicate) {

    def compile(): Fragment = Fragment.const(s"${elem.toString} WHERE") ++ parentheses(predicate)
  }

  private[internal] case class `andD`[T <: Product](elem: logicalOpD[T], predicate: Fragment)
      extends logicalOpD[T](elem, predicate) {

    def compile(): Fragment = elem.compile() ++ fr"AND" ++ parentheses(predicate)
  }

  private[internal] case class `orD`[T <: Product](elem: logicalOpD[T], predicate: Fragment)
      extends logicalOpD[T](elem, predicate) {

    def compile(): Fragment = elem.compile() ++ fr"OR" ++ parentheses(predicate)
  }
}
