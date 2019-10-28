package prepy.syntax.factory.doobie

import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.ast.{QueryElement, SelectSyntax}

trait DoobieSelect {
  implicit class `fromT`[T <: Product: Read](from: SelectSyntax.`fromT`) {
    def query(): Query0[T] = Query0(from.toString)
  }
  implicit class logicalOp[T <: Product: Read](logicalOp: SelectSyntax.logicalOp) {
    def query(): Query0[T] = Query0(logicalOp.toString)
  }
}
