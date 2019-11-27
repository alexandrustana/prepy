package prepy.operators

object Helper {

  implicit class inSyntax[T](operator1: T) {
    def in(operator2: List[T]): Boolean = operator2.contains(operator1)
  }

  implicit class likeSyntax(operator1: String) {
    def like(operator2: String): Boolean = operator2.contains(operator1)
  }

  implicit class betweenSyntax[T](operator1: T) {
    def between(operator2: T): andSyntax = andSyntax(operator2)

    case class andSyntax(operator2: T) {
      def and(operator3: T): Boolean = operator1 == operator2 && operator1 == operator3
    }
  }
}
