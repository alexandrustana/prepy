package prepy.syntax.doobie.internal

import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Select

private[doobie] trait DoobieSelect {

  implicit class `fromD`[O <: Product](elem: Select.`fromT`[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)

    def where(fr: Fragment): DoobieSelect.`whereD`[O] = DoobieSelect.`whereD`[O](elem, fr)
  }

  implicit class `selectFilterD`[O <: Product](elem: DoobieSelect.logicalOpD[_] { type Out = O })(
    implicit read:                                   Read[O]
  ) {
    def query(): Query0[O] = elem.compile().query[O]
  }
}

object DoobieSelect extends DoobieWhere {}
