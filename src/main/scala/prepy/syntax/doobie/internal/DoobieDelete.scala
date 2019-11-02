package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.fragment.Fragment
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Delete

private[doobie] trait DoobieDelete {

  implicit class `deleteD`[O <: Product](elem: Delete.`deleteT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)

    def where(fr: Fragment): DoobieDelete.`whereD`[O] = DoobieDelete.`whereD`[O](elem, fr)
  }

  implicit class `deleteFilterD`[O <: Product](elem: DoobieDelete.logicalOpD[_] { type Out = O })(
    implicit read:                                   Write[O]
  ) {
    def update(): Update0 = elem.compile().update
  }

}

object DoobieDelete extends DoobieWhere {}
