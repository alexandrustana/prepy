package prepy.convert

import org.specs2.mutable._
import prepy.convert.ops.syntax._
import prepy.convert.Domain.implicits._

class SyntaxTest extends Specification {
  case class Test(i: Int, j: Boolean)

  "Select" should {
    "simple" in {
      val sql = select[Test].from[Test].apply()

      sql === "SELECT i,j FROM Test"
    }
    "complex" in {
      val sql = select[Test].from[Test].where("1 == 1").apply()
      sql === "SELECT i,j FROM Test WHERE (1 == 1)"
    }
  }

  "Delete" should {
    "simple" in {
      val sql = delete[Test].apply()
      delete[Test].apply() === "DELETE FROM Test"
    }
    "complex" in {
      val sql = delete[Test].where("1 == 1").apply()
      sql === "DELETE FROM Test WHERE (1 == 1)"
    }
  }
}
