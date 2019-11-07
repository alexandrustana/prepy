package prepy.implicits.internal

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.FlatMapper

trait FlattenPoly {

  trait primitiveFieldName extends Poly1 {
    implicit def primitive[K <: Symbol, V](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], K :: HNil] = {
      at[FieldType[K, V]](_ => witness.value :: HNil)
    }
  }

  object flattenNestedNames extends primitiveFieldName {
    implicit def complex[K <: Symbol, V <: Product, GenV <: HList, FlatV <: HList](
      implicit
      gen:     LabelledGeneric.Aux[V, GenV],
      flatten: FlatMapper.Aux[flattenNestedNames.type, GenV, FlatV]
    ): Case.Aux[FieldType[K, V], FlatV] = {
      at[FieldType[K, V]](t => flatten(gen.to(t)))
    }
  }

  trait primitiveFieldType extends Poly1 {
    implicit def primitive[K <: Symbol, V](
      implicit
      witness: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], V :: HNil] = {
      at[FieldType[K, V]](t => t :: HNil)
    }
  }

  object flattenNestedTypes extends primitiveFieldType {
    implicit def complex[K <: Symbol, V <: Product, GenV <: HList, FlatV <: HList](
      implicit
      gen:     LabelledGeneric.Aux[V, GenV],
      flatten: FlatMapper.Aux[flattenNestedTypes.type, GenV, FlatV]
    ): Case.Aux[FieldType[K, V], FlatV] = {
      at[FieldType[K, V]](t => flatten(gen.to(t)))
    }
  }

}
