package prepy.implicits.internal

import prepy.implicits.Implicits
import shapeless._
import shapeless.ops.hlist.{FillWith, FlatMapper, ToList}

private[implicits] trait Serialize extends FlattenPoly {
  type Serialize[Entity <: Product]                 = Implicits.Serialize[Entity]

  private def pure[T <: Product](symbols: List[Symbol]): Serialize[T] = new Serialize[T] {
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
     flatMap:          FlatMapper.Aux[flattenNestedNames.type, EntityRepr, FlatEntityRepr],
     toList:           ToList[FlatEntityRepr, Symbol],
     fill:             FillWith[witnessPoly.type, FlatEntityRepr]
  ): Serialize[Entity] = {
    pure(toList(fill()))
  }
}
