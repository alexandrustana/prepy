package prepy.syntax.ast.internal

import shapeless.Coproduct
import shapeless.ops.coproduct.Selector

private[syntax] trait Where {
  private[syntax] trait logicalOp[O <: Product, T <: Coproduct] extends Query {
    type Out = O

    def queryElement: Query
    def condition:    String

    def and[S <: Product](condition: S => Boolean)(implicit selector: Selector[S, T]): `andT`[O, T] =
      `andT`[O, T](this, "and S => Boolean")

    def or[S <: Product](condition: S => Boolean)(implicit selector: Selector[S, T]): `orT`[O, T] =
      `orT`[O, T](this, "or S => Boolean")
  }
  private[syntax] case class `whereT`[O <: Product, T <: Coproduct](queryElement: Query, condition: String)
      extends logicalOp[O, T] {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  private[syntax] case class `andT`[O <: Product, T <: Coproduct](queryElement: Query, condition: String)
      extends logicalOp[O, T] {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[syntax] case class `orT`[O <: Product, T <: Coproduct](queryElement: Query, condition: String)
      extends logicalOp[O, T] {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
