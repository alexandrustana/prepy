package prepy.syntax.elements

private[prepy] trait WhereSyntax {
  private[prepy] case class `whereT`(queryElement: QueryElement, conditions: List[String]) extends QueryElement {
    override def toString: String =
      s"$queryElement WHERE ${conditions.map(condition => s"($condition)").mkString(" AND ")}"
  }
}
