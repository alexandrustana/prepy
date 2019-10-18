package prepy.convert.ops

import prepy.convert.Utils
import shapeless.ops.hlist._
import shapeless.{::, HList, HNil, _}
import doobie._
import doobie.implicits._
import cats.implicits._

trait Updatable extends Utils {
  type Entity

  val fields: List[Symbol]

  private def updateSql(table: Name): SQL =
    structure(table, fields) match {
      case (tableName, columnNames, holes) =>
        s"""UPDATE $tableName
           |SET
           | ${columnNames.tail
             .zip(holes.tail)
             .map { case (field, hole) => s"$field = $hole" }
             .mkString(", ")}
           |WHERE ${snakeCaseTransformation(columnNames.head)} = ?""".stripMargin
    }

  def update[Repr <: HList, H, T <: HList, Shifted <: HList](
    a:     Entity,
    table: Name
  )(
    implicit
    gen:     shapeless.LabelledGeneric.Aux[Entity, Repr],
    isHCons: IsHCons.Aux[Repr, H, T],
    shift:   Prepend.Aux[T, H :: HNil, Shifted],
    write:   Write[Shifted]
  ): Update0 = {
    val hList     = gen.to(a)
    val head      = isHCons.head(hList)
    val tail      = isHCons.tail(hList)
    val reordered = tail :+ head

    Update[Shifted](updateSql(table)).toUpdate0(reordered)
  }

  def update[Repr <: HList, H, T <: HList, Shifted <: HList](
    a: Entity
  )(
    implicit
    typeable: Typeable[Entity],
    gen:      shapeless.LabelledGeneric.Aux[Entity, Repr],
    isHCons:  IsHCons.Aux[Repr, H, T],
    shift:    Prepend.Aux[T, H :: HNil, Shifted],
    write:    Write[Shifted]
  ): Update0 = update(a, typeable.describe)

  def updateMany[Repr <: HList, H, T <: HList, Shifted <: HList](
    as: List[Entity]
  )(
    implicit
    typeable: Typeable[Entity],
    gen:      shapeless.LabelledGeneric.Aux[Entity, Repr],
    isHCons:  IsHCons.Aux[Repr, H, T],
    shift:    Prepend.Aux[T, H :: HNil, Shifted],
    write:    Write[Shifted]
  ): ConnectionIO[Int] = {
    val reordered: List[Shifted] = as.map(a => {
      val hList = gen.to(a)
      val head  = isHCons.head(hList)
      val tail  = isHCons.tail(hList)
      tail :+ head
    })

    Update[Shifted](updateSql(typeable.describe)).updateMany(reordered)
  }
}
