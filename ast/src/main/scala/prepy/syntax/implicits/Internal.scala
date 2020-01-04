package prepy.syntax.implicits

import prepy.syntax.implicits.internal.{Serialize, Validate}

import scala.annotation.implicitNotFound

trait Internal extends Serialize with Validate {}

object Internal {

  @implicitNotFound("Cannot build serialize function from ${T}")
  trait Serialize[T <: Product] {
    type Entity = T

    val fields: List[Symbol]
  }

  object Serialize {
    def apply[T <: Product](implicit inst: Serialize[T]): Serialize[T] = inst
  }

  @implicitNotFound(
    "Cannot build transform function from ${From} to ${To}, possibly due to missing fields in ${To} or because the fields in ${To} have different types"
  )
  trait Transform[From <: Product, To <: Product] {}

  object Transform {
    def apply[From <: Product, To <: Product](implicit inst: Transform[From, To]): Transform[From, To] = inst
  }
}
