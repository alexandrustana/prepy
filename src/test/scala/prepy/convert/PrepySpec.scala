package prepy.convert

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import org.specs2.mutable._
import prepy.syntax._
import prepy.Domain.implicits._

class PrepySpec extends Specification {
  case class Test(i: Int, j: Boolean)

  "select" should {
    case class ATable(i: Int, j: Boolean, k: String, l: Char, m: Double, n: Double, o: List[Int], p: Option[Float])
    case class BTable(i: Int, j: Boolean, k: String)
    case class CTable(i: Int, l: Char, o:    List[Int], p: Option[Float])
    case class DTable(a: Int, b: BTable, c:  String)
    case class ETable(d: Int, e: DTable, f:  String)

    "be equal" in {
      "select * query" in {
        "without condition" in {
          select[ATable].from[ATable].apply() mustEqual Valid("SELECT i, j, k, l, m, n, o, p FROM ATable")
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where("i == 1")
            .apply() mustEqual Valid("SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1)")
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .apply() mustEqual Valid("SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE)")
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .apply() mustEqual Valid("SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE)")
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].apply() mustEqual Valid("SELECT i, j, k FROM ATable")
        }
        "random fields" in {
          select[CTable].from[ATable].apply() mustEqual Valid("SELECT i, l, o, p FROM ATable")
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].apply() mustEqual Valid("SELECT a, i, j, k, c FROM ATable")
        }
        "two level nesting" in {
          select[ETable].from[ATable].apply() mustEqual Valid("SELECT d, a, i, j, k, c, f FROM ATable")
        }
      }

    }
    "be invalid" in {
      select[ATable].apply() mustEqual Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[T]`")
    }
  }

}
