package prepy.syntax.doobie.internal

import doobie.util.Write
import doobie.util.update.Update0
import prepy.syntax.ast.internal.Insert

private[doobie] trait DoobieInsert {

  implicit class `valueD`[O](elem: Insert.`valuesT`[_] { type Out = O })(implicit write: Write[O]) {
    def update(): Update0 = Update0(elem.toString, None)
  }

}
