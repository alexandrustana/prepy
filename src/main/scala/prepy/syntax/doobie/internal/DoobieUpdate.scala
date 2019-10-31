package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Update

private[doobie] trait DoobieUpdate {

  implicit class valuesUpdateSyntax[O](elem: Update.`setT`[_]{ type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)
  }

  implicit class logicalOpUpdateSyntax[O](elem: Update.logicalOp[_]{ type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)
  }
}
