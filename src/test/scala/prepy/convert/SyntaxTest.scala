package prepy.convert

import org.specs2.mutable._
import prepy.convert.ops.SelectSyntax._
import prepy.convert.Domain
import prepy.convert.Domain.implicits._

class SyntaxTest extends Specification {
  case class Test(i: Int, j: Boolean)

  "Select" should {
    "simple" in {
      select[Test].from[Test].apply() === "SELECT i,j FROM Test"
    }
    "complex" in {
      select[Test].from[Test].where("1 == 1") === "SELECT i,j FROM Test WHERE (1 == 1)"
    }
  }
}
