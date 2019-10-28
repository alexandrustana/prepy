package prepy.syntax.doobie

import doobie.util.update.Update0
import prepy.syntax.ast.internal.Delete

trait DoobieDelete {

  implicit class `fromT`[T <: Product](elem: Delete.`deleteT`) {
    def update(): Update0 = Update0(elem.toString, None)
  }

  implicit class logicalOp(elem: Delete.logicalOp) {
    def update(): Update0 = Update0(elem.toString, None)
  }

}
