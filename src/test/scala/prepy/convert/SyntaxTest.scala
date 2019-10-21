package prepy.convert

import org.specs2.mutable._
import prepy.convert.ops.Syntax._
import prepy.convert.Domain
import prepy.convert.Domain.implicits._

class SyntaxTest extends Specification {
  case class Test(i: Int, j: Boolean)

  "Select" should {
    "simple" in {
      val sql: String = select[Test].from[Test].apply()
      sql === "SELECT i,j FROM Test"
    }
    "complex" in {
      val sql: String = select[Test].from[Test].where("1 == 1").apply()
      sql === "SELECT i,j FROM Test WHERE (1 == 1)"
    }
  }

  "Delete" should {
    "simple" in {
      val sql: String = delete[Test].apply()
      delete[Test].apply() === "DELETE FROM Test"
    }
    "complex" in {
      val sql: String = delete[Test].where("1 == 1").apply()
      sql === "DELETE FROM Test WHERE (1 == 1)"
    }
  }
}
