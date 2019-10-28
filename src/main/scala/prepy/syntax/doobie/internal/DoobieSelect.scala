package prepy.syntax.doobie.internal

import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Select

private[doobie] trait DoobieSelect {

  implicit class fromSyntax[T <: Product : Read](from: Select.`fromT`) {
    def query(): Query0[T] = Query0(from.toString)
  }

  implicit class logicalOpSelectSyntax[T <: Product : Read](logicalOp: Select.logicalOp) {
    def query(): Query0[T] = Query0(logicalOp.toString)
  }

}
