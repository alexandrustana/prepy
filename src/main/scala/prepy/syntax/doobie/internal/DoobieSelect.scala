package prepy.syntax.doobie.internal

import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Select

private[doobie] trait DoobieSelect extends DoobieWhere {

  implicit class `fromD`[O <: Product](elem: Select.`fromT`[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)

    def where(fr: Fragment): `whereD`[O] = `whereD`[O](elem, fr)
  }

}
