package prepy

import shapeless._
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{FillWith, FlatMapper, ToList}

trait Domain[T] {
  type Entity = T

  val fields: List[Symbol]

  override def toString: String =
    s"""COLUMNS : $fields
       |""".stripMargin
}

object Domain {
  def apply[T](implicit inst: Domain[T]): Domain[T] = inst

  private def pure[T](symbols: List[Symbol]): Domain[T] = new Domain[T] {
    override val fields: List[Symbol] = symbols
  }

  object implicits {

    trait primitivePoly extends Poly1 {
      implicit def primitive[K <: Symbol, V](
        implicit
        witness: Witness.Aux[K]
      ): Case.Aux[FieldType[K, V], K :: HNil] = {
        at[FieldType[K, V]](t => witness.value :: HNil)
      }
    }

    object complexPoly extends primitivePoly {
      implicit def complex[K <: Symbol, V <: Product, GenV <: HList, FlatV <: HList](
        implicit
        witness: Witness.Aux[K],
        gen:     LabelledGeneric.Aux[V, GenV],
        flatten: FlatMapper.Aux[complexPoly.type, GenV, FlatV]
      ): Case.Aux[FieldType[K, V], FlatV] = {
        at[FieldType[K, V]](t => flatten(gen.to(t)))
      }
    }

    object witnessPoly extends Poly0 {
      implicit def default[T](implicit witness: Witness.Aux[T]): ProductCase.Aux[HNil, T] =
        at(witness.value)
    }

    implicit def toDomain[
      Entity,
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
}
