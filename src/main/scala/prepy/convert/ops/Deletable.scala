package prepy.convert.ops

import doobie._
import prepy.convert.Utils
import shapeless._

trait Deletable extends Utils {
  type Entity

  val fields: List[Symbol]

  private def deleteSql(table: Name): SQL =
    structure(table, fields) match {
      case (tableName, columnNames, holes) =>
        s"""DELETE FROM $tableName
           | WHERE ${columnNames.head} = ?""".stripMargin
    }

  def delete[T](id: T, table: Name)(implicit write: Write[T]): Update0 =
    Update[T](deleteSql(table)).toUpdate0(id)

  def delete[T](id: T)(implicit typeable: Typeable[Entity], write: Write[T]): Update0 = delete(id, typeable.describe)
}
