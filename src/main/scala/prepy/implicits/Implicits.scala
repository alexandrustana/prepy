package prepy.implicits

private[prepy] trait Implicits extends Serialize {}

object Implicits {

  trait Domain[T <: Product] {
    type Entity = T

    val fields: List[Symbol]

    override def toString: String =
      s"""COLUMNS : $fields
         |""".stripMargin
  }

  object Domain {
    def apply[T <: Product](implicit inst: Domain[T]): Domain[T] = inst
  }
}
