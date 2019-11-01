package prepy.syntax.doobie.internal

import doobie.implicits._
import doobie.util.fragment.Fragment
import doobie.util.fragments.parentheses
import prepy.syntax.ast.internal.{Query, Select}

private[internal] trait DoobieWhere {
  abstract private[internal] class logicalOp[T <: Product](elem: Query, predicate: Fragment)
      extends Select.logicalOp[T] {
    override def queryElement: Query  = elem
    override def condition:    String = predicate.toString
    override def toString:     String = elem.toString

    def compile(): Fragment

    def and(predicate: Fragment): `andD`[T] = `andD`[T](this, predicate)
    def or(predicate:  Fragment): `orD`[T]  = `orD`[T](this, predicate)
  }

  private[internal] case class `whereD`[T <: Product](elem: Query, predicate: Fragment)
      extends logicalOp[T](elem, predicate) {

    def compile(): Fragment = Fragment.const(s"${elem.toString} WHERE") ++ parentheses(predicate)
  }

  private[internal] case class `andD`[T <: Product](elem: logicalOp[T], predicate: Fragment)
      extends logicalOp[T](elem, predicate) {

    def compile(): Fragment = elem.compile() ++ fr"AND" ++ parentheses(predicate)
  }

  private[internal] case class `orD`[T <: Product](elem: logicalOp[T], predicate: Fragment)
      extends logicalOp[T](elem, predicate) {

    def compile(): Fragment = elem.compile() ++ fr"OR" ++ parentheses(predicate)
  }
}
