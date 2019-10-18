import prepy.convert.ops._
import shapeless.labelled.FieldType
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, _}

trait Table[A] extends Insertable with Updatable with Selectable with Deletable {
  type Entity = A

  val fields: List[Symbol]

  override def toString: String =
    s"""COLUMNS : $fields
       |""".stripMargin
}

object Table {
  def apply[C](implicit inst: Table[C]): Table[C] = inst

  def pure[C](fls: List[Symbol]): Table[C] = new Table[C] {
    override val fields: List[Symbol] = fls
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

    implicit def genTable[
      Domain,
      DomainRepr <: HList,
      FlatDomainRepr <: HList,
      SymbolRepr <: HList,
      FieldRepr <: HList
    ](
      implicit generic: LabelledGeneric.Aux[Domain, DomainRepr],
      flatMap:          FlatMapper.Aux[complexPoly.type, DomainRepr, FlatDomainRepr],
      toList:           ToList[FlatDomainRepr, Symbol],
      fill:             FillWith[witnessPoly.type, FlatDomainRepr]
    ): Table[Domain] = {
      pure(toList(fill()))
    }
  }
}

import Table.implicits._

case class EvenInner(n: Char)
case class Inner(k: String, m: EvenInner)
case class Test(i: Int, j: Boolean, l: Inner)

val gen = LabelledGeneric[Test]
val mapped = FlatMapper[complexPoly.type, gen.Repr]
val toList = ToList[mapped.Out, Symbol]
val fill = FillWith[witnessPoly.type, mapped.Out]
toList(fill())
