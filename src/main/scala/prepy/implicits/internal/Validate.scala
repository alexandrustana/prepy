package prepy.implicits.internal

import prepy.implicits.Implicits
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.{Generic, HList, HNil, LabelledGeneric}

trait Validate extends FlattenPoly {
  type Transform[From <: Product, To <: Product] = Implicits.Transform[From, To]

  implicit def validateTransformation[
    F <: Product,
    T <: Product,
    ReprF <: HList,
    ReprT <: HList,
    FlatF <: HList,
    FlatT <: HList,
    TDifF <: HList
  ](
    implicit fromGen: LabelledGeneric.Aux[F, ReprF],
    toGen:            LabelledGeneric.Aux[T, ReprT],
    flatFrom:         FlatMapper.Aux[complexPoly.type, ReprF, FlatF],
    flatTo:           FlatMapper.Aux[complexPoly.type, ReprT, FlatT],
    diff:             Diff.Aux[FlatT, FlatF, TDifF],
    isEmpty:          TDifF =:= HNil
  ): Transform[F, T] = new Transform[F, T] {}

}
