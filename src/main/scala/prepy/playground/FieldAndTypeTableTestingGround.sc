import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, _}

trait Table[C] {
  val columns: List[Symbol]
  val types:   List[String]

  override def toString: String =
    s"""COLUMNS : $columns
       |TYPES : $types
       |""".stripMargin
}

object Table {
  def apply[C](implicit inst: Table[C]): Table[C] = inst

  def pure[C](cols: List[Symbol], tps: List[String]): Table[C] = new Table[C] {
    override val columns: List[Symbol] = cols
    override val types:   List[String] = tps
  }

  object implicits {

    trait lowPriorityFlattener extends Poly1 {
      implicit def default[T]: Case.Aux[T, T :: HNil] = at[T](_ :: HNil)
    }

    object flattener extends lowPriorityFlattener {
      implicit def caseProduct[T <: Product, HL <: HList](
        implicit
        gen: Generic.Aux[T, HL],
        fm:  FlatMapper[flattener.type, HL]
      ): Case.Aux[T, fm.Out] = at[T] { t =>
        gen.to(t).flatMap(flattener)
      }
    }

    object typeablePoly extends Poly1 {
      implicit def default[A](
        implicit typeable: Typeable[A]
      ): Case.Aux[A, String] = at(_ => typeable.describe)
    }

    object nullPoly extends Poly0 {
      implicit def default[A]: ProductCase.Aux[HNil, A] =
        at(null.asInstanceOf[A])
    }

    implicit def genTable[
      Cols,
      ColsRepr <: HList,
      ColsFlatRepr <: HList,
      FieldRepr <: HList,
      TypeRepr <: HList,
      TypeFlatRepr <: HList,
      TypeString <: HList
    ](
      implicit gen: LabelledGeneric.Aux[Cols, ColsRepr],
      flatMapField: FlatMapper.Aux[flattener.type, ColsRepr, ColsFlatRepr],
      fields:       Keys.Aux[ColsFlatRepr, FieldRepr],
      fieldsToList: ToList[FieldRepr, Symbol],
      types:        Values.Aux[ColsRepr, TypeRepr],
      flatMapType:  FlatMapper.Aux[flattener.type, TypeRepr, TypeFlatRepr],
      mapper:       Mapper.Aux[typeablePoly.type, TypeFlatRepr, TypeString],
      fill:         FillWith[nullPoly.type, TypeFlatRepr],
      typesToList:  ToList[TypeString, String]
    ): Table[Cols] = {
      pure(fieldsToList(fields()), typesToList(mapper(fill())))
    }
  }
}

import Table.implicits._

case class Inner(k: String, m: Char)
case class Test(i: Int, j: Boolean, l: Inner)

Table[Test]