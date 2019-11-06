package prepy.implicits

import prepy.implicits.internal.FlattenPoly
import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FillWith, FlatMapper, ToList}

private[implicits] trait Serialize extends FlattenPoly {
  type Domain[T <: Product] = Implicits.Serialize[T]

  private def pure[T <: Product](symbols: List[Symbol]): Domain[T] = new Domain[T] {
    override val fields: List[Symbol] = symbols
  }

  object witnessPoly extends Poly0 {
    implicit def default[T](implicit witness: Witness.Aux[T]): ProductCase.Aux[HNil, T] =
      at(witness.value)
  }

  implicit def toDomain[
    Entity <: Product,
    EntityRepr <: HList,
    FlatEntityRepr <: HList,
    SymbolRepr <: HList,
    FieldRepr <: HList
  ](
    implicit generic: LabelledGeneric.Aux[Entity, EntityRepr],
    flatMap:          FlatMapper.Aux[complexPoly.type, EntityRepr, FlatEntityRepr],
    toList:           ToList[FlatEntityRepr, Symbol],
    fill:             FillWith[witnessPoly.type, FlatEntityRepr]
  ): Domain[Entity] = {
    pure(toList(fill()))
  }
}
