package prepy.convert

import org.specs2.mutable._
import prepy.syntax._
import prepy.Domain.implicits._

class SyntaxTest extends Specification {
  case class Test(i: Int, j: Boolean)

  "Select" should {
    "simple" in {
      val sql = select[Test].from[Test].apply()

      sql === "SELECT i, j FROM Test"
    }
    "complex" in {
      val sql = select[Test].from[Test].where("1 == 1").and("1 == 2").apply()
      sql === "SELECT i, j FROM Test WHERE (1 == 1) AND (1 == 2)"
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

  "Insert" should {
    "simple" in {
      val sql = insert[Test].values().apply()

      sql === "INSERT INTO Test (i, j) VALUES (?, ?)"
    }
  }

  "Update" should {
    "simple" in {
      val sql = update[Test].set[Test].where("1 == 1").apply()

      sql === "UPDATE Test SET i = ?, j = ? WHERE (1 == 1)"
    }
  }
}
