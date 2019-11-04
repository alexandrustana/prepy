package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.fragment.Fragment
import prepy.syntax.ast.internal.{Update => internal}

private[doobie] trait Update {

  implicit class `setD`[O <: Product](elem: internal.`setT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): doobie.Update[O] = doobie.Update[O](elem.toString, None)

    def where(fr: Fragment): Update.`whereD`[O] = Update.`whereD`[O](elem, fr)
  }

  implicit class `updateFilterD`[O <: Product](elem: Update.logicalOpD[_] { type Out = O })(
    implicit write:                                  Write[O]
  ) {
    def update(): doobie.Update[O] = doobie.Update[O](elem.compile().update.sql, None)
  }
}

object Update extends Where {}
