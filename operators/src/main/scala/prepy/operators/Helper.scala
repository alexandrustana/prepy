package prepy.operators

object Helper {

  implicit class inSyntax[T](operator1: T) {
    def in(operator2: List[T]): Boolean = operator2.contains(operator1)
  }

  implicit class likeSyntax(operator1: String) {
    def like(operator2: String): Boolean = operator2.contains(operator1)
  }
}
