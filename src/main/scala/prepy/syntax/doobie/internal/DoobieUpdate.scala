package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.fragment.Fragment
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Update

private[doobie] trait DoobieUpdate {

  implicit class valuesUpdateSyntax[O <: Product](elem: Update.`setT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)

    def where(fr: Fragment): DoobieUpdate.`whereD`[O] = DoobieUpdate.`whereD`[O](elem, fr)
  }

  implicit class `updateFilterD`[O <: Product](elem: DoobieUpdate.logicalOp[_] { type Out = O })(
    implicit read:                                   Write[O]
  ) {
    def update(): Update0 = elem.compile().update
  }
}

object DoobieUpdate extends DoobieWhere {}
