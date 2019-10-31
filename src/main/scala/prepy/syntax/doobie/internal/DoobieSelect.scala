package prepy.syntax.doobie.internal

import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Select

private[doobie] trait DoobieSelect {

  implicit class fromSyntax[O](elem: Select.`fromT`[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)
  }

  implicit class logicalOpSelectSyntax[O](elem: Select.logicalOp[_] { type Out = O })(implicit read: Read[O]) {
    def query(): Query0[O] = Query0[O](elem.toString)
  }

}
