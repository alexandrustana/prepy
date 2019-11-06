package prepy.implicits

import prepy.implicits.Implicits.Transform
import prepy.implicits.internal.FlattenPoly
import shapeless.{_0, HList, HNil, LabelledGeneric, Sized}
import shapeless.ops.hlist._

import scala.annotation.implicitNotFound

trait Validate extends FlattenPoly {

  implicit def validateTransformation[
    F <: Product,
    ReprF <: HList,
    FlatF <: HList,
    T <: Product,
    ReprT <: HList,
    FlatT <: HList,
    Diff <: HList
  ](
    implicit fromGen: LabelledGeneric.Aux[F, ReprF],
    toGen:            LabelledGeneric.Aux[T, ReprT],
    flatFrom:         FlatMapper.Aux[complexPoly.type, ReprF, FlatF],
    flatTo:           FlatMapper.Aux[complexPoly.type, ReprT, FlatT],
    diff:             Diff.Aux[FlatT, FlatF, Diff],
    empty:            HNil =:= Diff
  ): Transform[F, T] = new Transform[F, T] {}

}
