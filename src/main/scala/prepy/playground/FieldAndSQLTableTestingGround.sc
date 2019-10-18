import cats.data.OptionT
import cats.effect.IO
import doobie.free.connection.ConnectionIO
import prepy.convert.ops._
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

    trait lowPriorityFlattener extends Poly1 {
      println(this)
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

    implicit def genTable[
      Cols,
      ColsRepr <: HList,
      ColsFlatRepr <: HList,
      FieldRepr <: HList
    ](
      implicit gen: LabelledGeneric.Aux[Cols, ColsRepr],
      flatMapField: FlatMapper.Aux[flattener.type, ColsRepr, ColsFlatRepr],
      fields:       Keys.Aux[ColsFlatRepr, FieldRepr],
      fieldsToList: ToList[FieldRepr, Symbol]
    ): Table[Cols] = {
      pure(fieldsToList(fields()))
    }
  }
}