package prepy.implicits.internal

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.FlatMapper

private[implicits] trait FlattenPoly {

  trait primitivePoly extends Poly1 {
    implicit def primitive[K <: Symbol, V](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], K :: HNil] = {
      at[FieldType[K, V]](_ => witness.value :: HNil)
    }
  }

  object complexPoly extends primitivePoly {
    implicit def complex[K <: Symbol, V <: Product, GenV <: HList, FlatV <: HList](
      implicit
      gen:     LabelledGeneric.Aux[V, GenV],
      flatten: FlatMapper.Aux[complexPoly.type, GenV, FlatV]
    ): Case.Aux[FieldType[K, V], FlatV] = {
      at[FieldType[K, V]](t => flatten(gen.to(t)))
    }
  }
}
