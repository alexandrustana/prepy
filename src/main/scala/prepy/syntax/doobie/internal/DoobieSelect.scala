package prepy.syntax.doobie.internal

import doobie.implicits._
import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.Fragments.parentheses
import doobie.util.query.Query0
import prepy.syntax.ast.internal.{Query, Select}

private[doobie] trait DoobieSelect {

  implicit class fromSyntax[O <: Product](elem: Select.`fromT`[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)

    def where(fr: Fragment): `whereD`[O] = `whereD`[O](elem, fr)
  }

  abstract class logicalOp[T <: Product: Read](elem: Query, predicate: Fragment) extends Select.logicalOp[T] {
    override def queryElement: Query  = elem
    override def condition:    String = predicate.query[T].sql

    private[DoobieSelect] def compile(): Fragment

    def and(predicate: Fragment): `andD`[T] = `andD`[T](this, predicate)
    def or(predicate:  Fragment): `orD`[T]  = `orD`[T](this, predicate)

    def query(): Query0[T] = compile().query[T]
  }

  case class `whereD`[T <: Product: Read](elem: Query, predicate: Fragment) extends logicalOp[T](elem, predicate) {
    private[DoobieSelect] def compile(): Fragment = Fragment.const(s"${elem.toString} WHERE") ++ parentheses(predicate)
  }

  case class `andD`[T <: Product: Read](elem: logicalOp[T], predicate: Fragment) extends logicalOp[T](elem, predicate) {
    private[DoobieSelect] def compile(): Fragment = elem.compile() ++ fr"AND" ++ parentheses(predicate)
  }

  case class `orD`[T <: Product: Read](elem: logicalOp[T], predicate: Fragment) extends logicalOp[T](elem, predicate) {
    private[DoobieSelect] def compile(): Fragment = elem.compile() ++ fr"OR" ++ parentheses(predicate)
  }

}
