package prepy.syntax.factory.doobie

import doobie.util.Read
import doobie.util.query.Query0
import doobie.util.update.Update0
import prepy.syntax.ast.{DeleteSyntax, SelectSyntax}

trait DoobieDelete {

  implicit class `fromT`[T <: Product](elem: DeleteSyntax.`deleteT`) {
    def update(): Update0 = Update0(elem.toString, None)
  }

  implicit class logicalOp(elem: DeleteSyntax.logicalOp) {
    def update(): Update0 = Update0(elem.toString, None)
  }

}
