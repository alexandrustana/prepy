package prepy.syntax.doobie.internal

import doobie.util.update.Update0
import prepy.syntax.ast.internal.Delete

trait DoobieDelete {

  implicit class deleteSyntax[T <: Product](elem: Delete.`deleteT`) {
    def update(): Update0 = Update0(elem.toString, None)
  }

  implicit class logicalOpDeleteSyntax(elem: Delete.logicalOp) {
    def update(): Update0 = Update0(elem.toString, None)
  }

}
