package prepy.spec.syntax

import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.PrepyDomain
import prepy.syntax._

class SyntaxSpec extends Specification with PrepyDomain {

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
          select[DTable].from[ATable].apply() mustEqual Valid("SELECT l, i, j, k, m FROM ATable")
        }
        "two level nesting" in {
          select[ETable].from[ATable].apply() mustEqual Valid("SELECT n, l, i, j, k, m, o FROM ATable")
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
            "INSERT INTO DTable (l, i, j, k, m) VALUES (?, ?, ?, ?, ?)"
          )
        }
        "two level nesting" in {
          insert[ETable].values[ETable].apply() mustEqual Valid(
            "INSERT INTO ETable (n, l, i, j, k, m, o) VALUES (?, ?, ?, ?, ?, ?, ?)"
          )
        }
      }
    }
    "be invalid" in {
      insert[ATable].apply() mustEqual Invalid("Incomplete SQL query. `insert[T]` must be followed by a `values[K]`")
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
          update[ATable].set[DTable].apply() mustEqual Valid("UPDATE ATable SET l = ?, i = ?, j = ?, k = ?, m = ?")
        }
        "two level nesting" in {
          update[ATable].set[ETable].apply() mustEqual Valid(
            "UPDATE ATable SET n = ?, l = ?, i = ?, j = ?, k = ?, m = ?, o = ?"
          )
        }
      }

    }
    "be invalid" in {
      update[ATable].apply() mustEqual Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")
    }
  }
}
