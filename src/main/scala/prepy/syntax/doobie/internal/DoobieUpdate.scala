package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Update

private[doobie] trait DoobieUpdate {

  implicit class valuesUpdateSyntax[T <: Product: Write](elem: Update.`setT`) {
    def update(): Update0 = Update0(elem.toString, None)
  }

  implicit class logicalOpUpdateSyntax[T <: Product: Write](elem: Update.logicalOp) {
    def update(): Update0 = Update0(elem.toString, None)
  }
}
