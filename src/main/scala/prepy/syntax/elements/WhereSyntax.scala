package prepy.syntax.elements

private[prepy] trait WhereSyntax {
  private[prepy] case class `whereT`(queryElement: QueryElement, condition: List[String]) extends QueryElement {
    override def toString: String = s"$queryElement WHERE ${condition.mkString("(", "AND", ")")}"
  }
}
