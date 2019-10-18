package prepy.convert.ops

import doobie._
import doobie.implicits._
import cats.implicits._
import Fragments._
import prepy.convert.Utils
import shapeless._

trait Selectable extends Utils {
  type Entity

  val fields: List[Symbol]

  def selectId[T: Put](id: T)(implicit read: Read[Entity], typeable: Typeable[Entity]): Query0[Entity] =
    structure(typeable.describe, fields) match {
      case (name, columns, _) =>
        (Fragment.const(s"""SELECT ${columns.mkString(",")} FROM "$name"""") ++ whereAnd(
          Fragment.const(columns.head) ++ fr"= $id"
        )).query[Entity]
    }

  def selectIds[T: Put](ids: List[T])(implicit read: Read[Entity], typeable: Typeable[Entity]): Query0[Entity] =
    structure(typeable.describe, fields) match {
      case (name, columns, _) =>
        (Fragment.const(s"""SELECT ${columns.mkString(",")} FROM "$name"""") ++ whereAndOpt(
          ids.toNel.map(id => in(Fragment.const(columns.head), id))
        )).query[Entity]
    }

  def select(condition: Fragment*)(implicit read: Read[Entity], typeable: Typeable[Entity]): Query0[Entity] =
    selectFrom[Entity](condition: _*)

  def selectFrom[T](condition: Fragment*)(implicit read: Read[Entity], typeable: Typeable[T]): Query0[Entity] =
    structure(typeable.describe, fields) match {
      case (name, columns, _) =>
        (Fragment.const(s"""SELECT ${columns.mkString(",")} FROM "$name"""") ++ whereAnd(condition: _*)).query[Entity]
    }

  def selectOpt(condition: Option[Fragment]*)(implicit read: Read[Entity], typeable: Typeable[Entity]): Query0[Entity] =
    selectOptFrom[Entity](condition: _*)

  def selectOptFrom[T](
    condition:     Option[Fragment]*
  )(implicit read: Read[Entity], typeable: Typeable[T]): Query0[Entity] =
    structure(typeable.describe, fields) match {
      case (name, columns, _) =>
        (Fragment.const(s"""SELECT ${columns.mkString(",")} FROM "$name"""") ++ whereAndOpt(condition: _*))
          .query[Entity]
    }
}
