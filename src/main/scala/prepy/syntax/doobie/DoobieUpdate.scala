package prepy.syntax.doobie

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Update

trait DoobieUpdate {

  implicit class `valuesT`[T <: Product: Write](from: Update.`setT`) {
    def update(): Update0 = Update0(from.toString, None)
  }

  implicit class logicalOp[T <: Product: Write](from: Update.logicalOp) {
    def update(): Update0 = Update0(from.toString, None)
  }
}
