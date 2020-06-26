package prepy.syntax.implicits.internal

import prepy.syntax.implicits.Internal
import shapeless.ops.hlist._
import shapeless.{HList, HNil, LabelledGeneric}

trait ValidateInvoke extends LowPriorityInvoke {
  implicit def identityTransformation[From <: Product, To <: Product](implicit eq: From =:= To): Validate[From, To] =
    new Validate[From, To] {}
}

trait LowPriorityInvoke extends FlattenPoly {
  type Validate[From <: Product, To <: Product] = Internal.Validate[From, To]

  implicit def validateTransformation[
    From <: Product,
    To <: Product,
    ReprFrom <: HList,
    ReprTo <: HList,
    FlatFromNames <: HList,
    FlatToNames <: HList,
    FlatFromTypes <: HList,
    FlatToTypes <: HList,
    DiffNames <: HList,
    DiffTypes <: HList
  ](
    implicit genFrom: LabelledGeneric.Aux[From, ReprFrom],
    genTo:            LabelledGeneric.Aux[To, ReprTo],
    flatNamesFrom:    FlatMapper.Aux[flattenNestedNames.type, ReprFrom, FlatFromNames],
    flatNamesTo:      FlatMapper.Aux[flattenNestedNames.type, ReprTo, FlatToNames],
    flatTypesFrom:    FlatMapper.Aux[flattenNestedTypes.type, ReprFrom, FlatFromTypes],
    flatTypesTo:      FlatMapper.Aux[flattenNestedTypes.type, ReprTo, FlatToTypes],
    differentNames:   Diff.Aux[FlatToNames, FlatFromNames, DiffNames],
    sameFieldNames:   DiffNames =:= HNil,
    differentTypes:   Diff.Aux[FlatToTypes, FlatFromTypes, DiffTypes],
    sameFieldTypes:   DiffTypes =:= HNil
  ): Validate[From, To] = new Validate[From, To] {}

}
