package prepy.convert.ops

import doobie._
import prepy.convert.Utils
import shapeless._

trait Insertable extends Utils {
  type Entity

  val fields: List[Symbol]

  private def insertSql(table: Name): SQL =
    structure(table, fields) match {
      case (tableName, columnNames, holes) =>
        s"""INSERT INTO $tableName (${columnNames.mkString(", ")})
           | VALUES (${holes.mkString(", ")})""".stripMargin
    }

  def insert(a: Entity, table: Name)(implicit write: Write[Entity]): Update0 =
    Update[Entity](insertSql(table)).toUpdate0(a)

  def insert(a: Entity)(implicit typeable: Typeable[Entity], write: Write[Entity]): Update0 =
    insert(a, typeable.describe)
}
