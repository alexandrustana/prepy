package prepy.convert.ops.syntax.inner

private[syntax] trait WhereSyntax {
  case class `whereT`(queryElement: QueryElement, condition: List[String]) {
    def apply(): String = toString

    override def toString: String = s"$queryElement WHERE ${condition.mkString("(", "AND", ")")}"
  }
}
