package prepy.syntax.doobie.internal

import doobie.implicits._
import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.Fragments.parentheses
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Query

private[internal] trait DoobieWhere {
  abstract private[internal] class logicalOp[T <: Product: Read](elem: Query, predicate: Fragment) extends Query {
    override def toString: String = elem.toString

    private[DoobieWhere] def compile(): Fragment

    def and(predicate: Fragment): `andD`[T] = `andD`[T](this, predicate)
    def or(predicate:  Fragment): `orD`[T]  = `orD`[T](this, predicate)

    def query(): Query0[T] = compile().query[T]
  }

  private[internal] case class `whereD`[T <: Product: Read](elem: Query, predicate: Fragment)
      extends logicalOp[T](elem, predicate) {
    override def toString: String = s"${elem.toString} WHERE (${predicate.query[T].sql})"

    private[DoobieWhere] def compile(): Fragment = Fragment.const(s"${elem.toString} WHERE") ++ parentheses(predicate)
  }

  private[internal] case class `andD`[T <: Product: Read](elem: logicalOp[T], predicate: Fragment)
      extends logicalOp[T](elem, predicate) {
    override def toString: String = s"${elem.toString} AND (${predicate.query[T].sql})"

    private[DoobieWhere] def compile(): Fragment = elem.compile() ++ fr"AND" ++ parentheses(predicate)
  }

  private[internal] case class `orD`[T <: Product: Read](elem: logicalOp[T], predicate: Fragment)
      extends logicalOp[T](elem, predicate) {
    override def toString: String = s"${elem.toString} OR (${predicate.query[T].sql})"

    private[DoobieWhere] def compile(): Fragment = elem.compile() ++ fr"OR" ++ parentheses(predicate)
  }
}
