package prepy.syntax.factory.doobie

import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.ast.SelectSyntax
import prepy.syntax.factory.GenericFactory

object DoobieFactory extends GenericFactory {
  implicit class DoobieSelectFrom[T <: Product: Read](from: SelectSyntax.`fromT`) {
    def doobie(): Query0[T] = Query0(from.toString)
  }
  implicit class DoobieSelectLogical[T <: Product: Read](logicalOp: SelectSyntax.logicalOp) {
    def doobie(): Query0[T] = Query0(logicalOp.toString)
  }
}
