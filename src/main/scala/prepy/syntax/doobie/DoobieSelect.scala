package prepy.syntax.doobie

import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.ast.internal.Select

trait DoobieSelect {

  implicit class fromQ[T <: Product : Read](from: Select.`fromT`) {
    def query(): Query0[T] = Query0(from.toString)
  }

  implicit class logicalOp[T <: Product : Read](logicalOp: Select.logicalOp) {
    def query(): Query0[T] = Query0(logicalOp.toString)
  }

}
