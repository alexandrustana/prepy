package prepy.spec.syntax

import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.TestDomain
import prepy._

class SyntaxSpec extends Specification with TestDomain with TestImplicits {

  "select" should {

    "be equal" in {
      "select query" in {
        "without condition" in {
          select[ATable].from[ATable].apply() mustEqual Valid(
            "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable"
          )
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where(f => f.iField == 1)
            .apply() mustEqual Valid(
            "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid(
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE)"
            )
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid(
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) OR (jField == TRUE)"
            )
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
            )
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].apply() mustEqual Valid("SELECT iField, jField, kField FROM ATable")
        }
        "random fields" in {
          select[CTable].from[ATable].apply() mustEqual Valid("SELECT iField, lField, oField, pField FROM ATable")
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].apply() mustEqual Valid(
            "SELECT lField, iField, jField, kField, mField FROM ATable"
          )
        }
        "two level nesting" in {
          select[ETable].from[ATable].apply() mustEqual Valid(
            "SELECT nField, lField, iField, jField, kField, mField, oField FROM ATable"
          )
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
            .where(f => f.iField == 1)
            .apply() mustEqual Valid("DELETE FROM ATable WHERE (iField == 1)")
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid("DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE)")
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            delete[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid("DELETE FROM ATable WHERE (iField == 1) OR (jField == TRUE)")
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
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
          "INSERT INTO ATable (iField, jField, kField, lField, mField, nField, oField, pField) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
      }
      "insert all fields from nested product" in {
        "one level nesting" in {
          insert[ATable].values[DTable].apply() mustEqual Valid(
            "INSERT INTO ATable (lField, iField, jField, kField, mField) VALUES (?, ?, ?, ?, ?)"
          )
        }
        "two level nesting" in {
          insert[ATable].values[ETable].apply() mustEqual Valid(
            "INSERT INTO ATable (nField, lField, iField, jField, kField, mField, oField) VALUES (?, ?, ?, ?, ?, ?, ?)"
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
            "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ?"
          )
        }

        "with single condition" in {
          update[ATable]
            .set[ATable]
            .where(f => f.iField == 1)
            .apply() mustEqual Valid(
            "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid(
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE)"
            )
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid(
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) OR (jField == TRUE)"
            )
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
            )
          }
        }
      }

      "update subset query" in {
        "first three fields" in {
          update[ATable].set[BTable].apply() mustEqual Valid("UPDATE ATable SET iField = ?, jField = ?, kField = ?")
        }
        "random fields" in {
          update[ATable].set[CTable].apply() mustEqual Valid(
            "UPDATE ATable SET iField = ?, lField = ?, oField = ?, pField = ?"
          )
        }
      }

      "update from nested product" in {
        "one level nesting" in {
          update[ATable].set[DTable].apply() mustEqual Valid(
            "UPDATE ATable SET lField = ?, iField = ?, jField = ?, kField = ?, mField = ?"
          )
        }
        "two level nesting" in {
          update[ATable].set[ETable].apply() mustEqual Valid(
            "UPDATE ATable SET nField = ?, lField = ?, iField = ?, jField = ?, kField = ?, mField = ?, oField = ?"
          )
        }
      }

    }
    "be invalid" in {
      update[ATable].apply() mustEqual Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")
    }
  }
}
