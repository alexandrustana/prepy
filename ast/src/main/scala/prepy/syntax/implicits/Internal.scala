package prepy.syntax.implicits

import prepy.syntax.implicits.internal._

import scala.annotation.implicitNotFound

trait Internal extends SerializeInvoke with ValidateInvoke with TransformInvoke {}

object Internal {

  @implicitNotFound("Cannot build serialize type class for ${T}")
  trait Serialize[T <: Product] {
    type Entity = T

    val fields: List[Symbol]
    val name:   String
  }

  object Serialize {
    def apply[T <: Product](implicit inst: Serialize[T]): Serialize[T] = inst
  }

  @implicitNotFound(
    "Cannot build transform validate type class for ${From} and ${To}, possibly due to missing fields in ${To} or because the fields in ${To} have different types"
  )
  trait Validate[From <: Product, To <: Product] {}

  object Validate {
    def apply[From <: Product, To <: Product](implicit inst: Validate[From, To]): Validate[From, To] = inst
    def dummy[From <: Product, To <: Product]: Validate[From, To] = new Validate[From, To] {}
  }

  type IdentityTransform[T <: Product] = Transform[T, T]

  trait Transform[From <: Product, To <: Product] {
    def from: Serialize[From]
    def to:   Serialize[To]
  }

  object Transform {
    def apply[From <: Product, To <: Product](implicit inst: Transform[From, To]): Transform[From, To] = inst
  }
}
