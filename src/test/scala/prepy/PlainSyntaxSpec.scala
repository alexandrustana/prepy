package prepy

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.syntax._
import shapeless.cachedImplicit

class PlainSyntaxSpec extends Specification {

  case class ATable(i: Int, j: Boolean, k: String, l: Char, m: Double, n: Double, o: List[Int], p: Option[Float])
  case class BTable(i: Int, j: Boolean, k: String)
  case class CTable(i: Int, l: Char, o:    List[Int], p: Option[Float])
  case class DTable(a: Int, b: BTable, c:  String)
  case class ETable(d: Int, e: DTable, f:  String)

  implicit val aDomain = cachedImplicit[Domain[ATable]]
  implicit val bDomain = cachedImplicit[Domain[BTable]]
  implicit val cDomain = cachedImplicit[Domain[CTable]]
  implicit val dDomain = cachedImplicit[Domain[DTable]]
  implicit val EDomain = cachedImplicit[Domain[ETable]]

  "select" should {

    "be equal" in {
      "select query" in {
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
      select[ATable].apply() mustEqual Invalid("Incomplete SQL query. `select[T]` must be followed by a `from[K]`")
    }
  }

  "delete" should {
    "be equal" in {
      "delete from" in {
        "without condition" in {
          delete[ATable].apply() mustEqual Valid("DELETE FROM ATable")
        }
        "with single condition" in {
          delete[ATable]
            .where("i == 1")
            .apply() mustEqual Valid("DELETE FROM ATable WHERE (i == 1)")
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .apply() mustEqual Valid("DELETE FROM ATable WHERE (i == 1) AND (j == TRUE)")
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .apply() mustEqual Valid("DELETE FROM ATable WHERE (i == 1) OR (j == TRUE)")
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
        }
      }
    }
  }

  "insert" should {
    "be equal" in {
      "insert all fields" in {
        insert[ATable].values[ATable].apply() mustEqual Valid(
          "INSERT INTO ATable (i, j, k, l, m, n, o, p) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
      }
      "insert all fields from nested product" in {
        "one level nesting" in {
          insert[DTable].values[DTable].apply() mustEqual Valid(
            "INSERT INTO DTable (a, i, j, k, c) VALUES (?, ?, ?, ?, ?)"
          )
        }
        "two level nesting" in {
          insert[ETable].values[ETable].apply() mustEqual Valid(
            "INSERT INTO ETable (d, a, i, j, k, c, f) VALUES (?, ?, ?, ?, ?, ?, ?)"
          )
        }
      }
    }
    "be invalid" in {
      insert[ATable].apply() mustEqual Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values`")
    }
  }

  "update" should {

    "be equal" in {
      "update all query" in {
        "without condition" in {
          update[ATable].set[ATable].apply() mustEqual Valid(
            "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ?"
          )
        }

        "with single condition" in {
          update[ATable]
            .set[ATable]
            .where("i == 1")
            .apply() mustEqual Valid(
            "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .apply() mustEqual Valid(
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE)"
            )
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .apply() mustEqual Valid(
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) OR (j == TRUE)"
            )
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .apply() mustEqual Valid(
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
            )
          }
        }
      }

      "update subset query" in {
        "first three fields" in {
          update[ATable].set[BTable].apply() mustEqual Valid("UPDATE ATable SET i = ?, j = ?, k = ?")
        }
        "random fields" in {
          update[ATable].set[CTable].apply() mustEqual Valid("UPDATE ATable SET i = ?, l = ?, o = ?, p = ?")
        }
      }

      "update from nested product" in {
        "one level nesting" in {
          update[ATable].set[DTable].apply() mustEqual Valid("UPDATE ATable SET a = ?, i = ?, j = ?, k = ?, c = ?")
        }
        "two level nesting" in {
          update[ATable].set[ETable].apply() mustEqual Valid(
            "UPDATE ATable SET d = ?, a = ?, i = ?, j = ?, k = ?, c = ?, f = ?"
          )
        }
      }

    }
    "be invalid" in {
      update[ATable].apply() mustEqual Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[T]`")
    }
  }
}
