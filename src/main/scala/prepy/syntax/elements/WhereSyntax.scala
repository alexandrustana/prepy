package prepy.syntax.elements

private[prepy] trait WhereSyntax {
  private[prepy] case class `whereT`(queryElement: QueryElement, condition: String) extends QueryElement {
    def and(condition: String): `andT` = `andT`(this, condition)
    def or(condition:  String): `orT`  = `orT`(this, condition)

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }

  abstract private[prepy] class LogicalOp(queryElement: QueryElement, condition: String) extends QueryElement {
    def and(condition: String): `andT` = `andT`(this, condition)
    def or(condition:  String): `orT`  = `orT`(this, condition)
  }

  private[prepy] case class `andT`(queryElement: QueryElement, condition: String)
      extends LogicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[prepy] case class `orT`(queryElement: QueryElement, condition: String)
      extends LogicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
