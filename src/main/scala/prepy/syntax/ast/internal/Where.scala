package prepy.syntax.ast.internal

private[syntax] trait Where {
  private[syntax] trait logicalOp[T <: Product] extends Query {
    type Out = T

    def queryElement: Query
    def condition:    String

    def and(condition: String): `andT`[T] = `andT`[T](this, condition)
    def or(condition:  String): `orT`[T]  = `orT`[T](this, condition)
  }
  private[syntax] case class `whereT`[T <: Product](queryElement: Query, condition: String) extends logicalOp[T] {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  private[syntax] case class `andT`[T <: Product](queryElement: Query, condition: String) extends logicalOp[T] {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[syntax] case class `orT`[T <: Product](queryElement: Query, condition: String) extends logicalOp[T] {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
