package prepy.syntax.implicits.internal

import prepy.syntax.implicits.Internal

trait TransformInvoke extends SerializeInvoke with ValidateInvoke {
  type Transform[From <: Product, To <: Product] = Internal.Transform[From, To]

  implicit def apply[From <: Product, To <: Product](
    implicit validate: Validate[From, To],
    serializeFrom:     Serialize[From],
    serializeTo:       Serialize[To]
  ): Transform[From, To] = new Transform[From, To] {
    override def from: Serialize[From] = serializeFrom
    override def to:   Serialize[To]   = serializeTo
  }
}
