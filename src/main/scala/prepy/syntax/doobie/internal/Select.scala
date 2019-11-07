package prepy.syntax.doobie.internal

import doobie.util.Read
import doobie.util.fragment.Fragment
import doobie.util.query.Query0
import prepy.syntax.ast.internal.{Select => internal}

private[doobie] trait Select {

  implicit class `fromD`[O <: Product](elem: internal.`fromT`[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)

    def where(fr: Fragment): Select.`whereD`[O] = Select.`whereD`[O](elem, fr)

    def where(fr: Option[Fragment]): Select.`whereD`[O] = new Select.`whereD`[O](elem, fr)
  }

  implicit class `selectFilterD`[O <: Product](elem: Select.logicalOpD[_] { type Out = O })(
    implicit read:                                   Read[O]
  ) {
    def query(): Query0[O] = elem.compile().query[O]
  }
}

object Select extends Where {}
