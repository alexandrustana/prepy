package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.fragment.Fragment
import doobie.util.update.Update0
import prepy.syntax.ast.internal.{Update => internal}

private[doobie] trait Update {

  implicit class `setD`[O <: Product](elem: internal.`setT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)

    def where(fr: Fragment): Update.`whereD`[O] = Update.`whereD`[O](elem, fr)
  }

  implicit class `updateFilterD`[O <: Product](elem: Update.logicalOpD[_] { type Out = O })(
    implicit write:                                  Write[O]
  ) {
    def update(): Update0 = elem.compile().update
  }
}

object Update extends Where {}
