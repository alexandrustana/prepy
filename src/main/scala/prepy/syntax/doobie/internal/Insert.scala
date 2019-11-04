package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.{Insert => internal}

private[doobie] trait Insert {

  implicit class `valueD`[O](elem: internal.`valuesT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): doobie.Update[O] = doobie.Update[O](elem.toString, None)
  }

}
