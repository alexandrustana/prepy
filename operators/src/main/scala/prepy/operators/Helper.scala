package prepy.operators

object Helper {

  implicit class inSyntax[T](operator1: T) {
    def in(operator2: List[T]): Boolean = operator2.contains(operator1)
  }

}
