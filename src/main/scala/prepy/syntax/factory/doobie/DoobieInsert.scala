package prepy.syntax.factory.doobie

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.{InsertSyntax, SelectSyntax}

trait DoobieInsert {

  implicit class `valuesT`[T <: Product: Write](from: InsertSyntax.`valuesT`) {
    def update(): Update0 = Update0(from.toString, None)
  }

}
