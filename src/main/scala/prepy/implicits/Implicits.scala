package prepy.implicits

import scala.annotation.implicitNotFound

private[prepy] trait Implicits extends Serialize with Validate {}

object Implicits {

  @implicitNotFound("Cannot build serialize function from ${T}")
  trait Serialize[T <: Product] {
    type Entity = T

    val fields: List[Symbol]

    override def toString: String =
      s"""COLUMNS : $fields
         |""".stripMargin
  }

  object Serialize {
    def apply[T <: Product](implicit inst: Serialize[T]): Serialize[T] = inst
  }

  @implicitNotFound("Cannot build transform function from ${From} to ${To}, possibly due to missing fields in ${To}")
  trait Transform[From <: Product, To <: Product]{}

  object Transform {
    def apply[From <: Product, To <: Product](implicit  inst: Transform[From, To]): Transform[From, To] = inst
  }
}
