package prepy.syntax.implicits.internal

import prepy.syntax.implicits.Internal
import shapeless._
import shapeless.ops.hlist.{FillWith, FlatMapper, ToList}

import scala.reflect.ClassTag

private[implicits] trait SerializeInvoke extends FlattenPoly {
  type ClassName                    = String
  type Serialize[Entity <: Product] = Internal.Serialize[Entity]

  private def pure[T <: Product](symbols: List[Symbol])(implicit tag: ClassTag[T]): Serialize[T] = new Serialize[T] {
    override val fields: List[Symbol] = symbols
    override val name:   String       = tag.runtimeClass.getSimpleName
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
    fill:             FillWith[witnessPoly.type, FlatEntityRepr],
    tag:              ClassTag[Entity]
  ): Serialize[Entity] = {
    pure[Entity](toList(fill()))
  }
}
