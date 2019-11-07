package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.fragment.Fragment
import doobie.util.update.Update0
import prepy.syntax.ast.internal.{Delete => internal}

private[doobie] trait Delete {

  implicit class `deleteD`[O <: Product](elem: internal.`deleteT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)

    def where(fr: Fragment): Delete.`whereD`[O] = Delete.`whereD`[O](elem, fr)

    def where(fr: Option[Fragment]): Delete.`whereD`[O] = new Delete.`whereD`[O](elem, fr)
  }

  implicit class `deleteFilterD`[O <: Product](elem: Delete.logicalOpD[_] { type Out = O })(
    implicit read:                                   Write[O]
  ) {
    def update(): Update0 = elem.compile().update
  }

}

object Delete extends Where {}
