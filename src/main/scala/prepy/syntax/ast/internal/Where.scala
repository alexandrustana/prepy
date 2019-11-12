package prepy.syntax.ast.internal

private[syntax] trait Where {
  private[syntax] trait logicalOp[O <: Product, T <: Product] extends Query {
    type Out = O

    def queryElement: Query
    def condition:    String

    def and(condition: String): `andT`[O, T] = `andT`[O, T](this, condition)
    def or(condition:  String): `orT`[O, T]  = `orT`[O, T](this, condition)
  }
  private[syntax] case class `whereT`[O <: Product, T <: Product](queryElement: Query, condition: String)
      extends logicalOp[O, T] {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  private[syntax] case class `andT`[O <: Product, T <: Product](queryElement: Query, condition: String)
      extends logicalOp[O, T] {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[syntax] case class `orT`[O <: Product, T <: Product](queryElement: Query, condition: String)
      extends logicalOp[O, T] {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
