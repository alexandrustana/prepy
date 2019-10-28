package prepy.syntax.factory.doobie

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.UpdateSyntax

trait DoobieUpdate {

  implicit class `valuesT`[T <: Product: Write](from: UpdateSyntax.`setT`) {
    def update(): Update0 = Update0(from.toString, None)
  }

  implicit class logicalOp[T <: Product: Write](from: UpdateSyntax.logicalOp) {
    def update(): Update0 = Update0(from.toString, None)
  }
}
