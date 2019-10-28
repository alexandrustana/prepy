package prepy.syntax.ast.internal

trait Where {
  abstract private[syntax] class logicalOp(queryElement: Query, condition: String) extends Query {
    def and(condition: String): `andT` = `andT`(this, condition)
    def or(condition:  String): `orT`  = `orT`(this, condition)
  }
  private[syntax] case class `whereT`(queryElement: Query, condition: String)
    extends logicalOp(queryElement, condition) {

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }
  private[syntax] case class `andT`(queryElement: Query, condition: String) extends logicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[syntax] case class `orT`(queryElement: Query, condition: String) extends logicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
