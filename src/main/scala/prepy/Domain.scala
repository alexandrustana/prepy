package prepy

trait Domain[T] {
  type Entity = T

  val fields: List[Symbol]

  override def toString: String =
    s"""COLUMNS : $fields
       |""".stripMargin
}

object Domain {
  def apply[T](implicit inst: Domain[T]): Domain[T] = inst
}
