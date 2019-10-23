package prepy.syntax.plain

private[prepy] trait WhereSyntax {
  private[syntax] case class `whereT`(queryElement: QueryElement, condition: String) extends QueryElement {
    def and(condition: String): `andT` = `andT`(this, condition)
    def or(condition:  String): `orT`  = `orT`(this, condition)

    override def toString: String =
      s"$queryElement WHERE (${condition})"
  }

  abstract private[syntax] class logicalOp(queryElement: QueryElement, condition: String) extends QueryElement {
    def and(condition: String): `andT` = `andT`(this, condition)
    def or(condition:  String): `orT`  = `orT`(this, condition)
  }

  private[syntax] case class `andT`(queryElement: QueryElement, condition: String)
      extends logicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement AND ($condition)"
  }
  private[syntax] case class `orT`(queryElement: QueryElement, condition: String)
      extends logicalOp(queryElement, condition) {
    override def toString: String = s"$queryElement OR ($condition)"
  }
}
