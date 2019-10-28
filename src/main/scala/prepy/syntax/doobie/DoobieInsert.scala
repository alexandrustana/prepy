package prepy.syntax.doobie

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Insert

trait DoobieInsert {

  implicit class `valuesT`[T <: Product: Write](from: Insert.`valuesT`) {
    def update(): Update0 = Update0(from.toString, None)
  }

}
