package prepy.convert.ops.syntax.inner

private[syntax] trait WhereSyntax {
  case class `whereT`(queryElement: QueryElement, condition: List[String]) extends QueryElement {
    override def toString: String = s"$queryElement WHERE ${condition.mkString("(", "AND", ")")}"
  }
}
