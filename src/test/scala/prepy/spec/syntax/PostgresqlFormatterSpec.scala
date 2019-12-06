package prepy.spec.syntax

import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.TestDomain
import prepy.formatter.postgresql._
import prepy._

class PostgresqlFormatterSpec extends Specification with TestDomain with TestImplicits {

  "select" should {

    "be equal" in {
      "select query" in {
        "without condition" in {
          select[ATable].from[ATable].apply() mustEqual Valid(
            "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table"
          )
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where(f => f.iField == 1)
            .apply() mustEqual Valid(
            "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) AND (j_field == TRUE)"
            )
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) OR (j_field == TRUE)"
            )
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].apply() mustEqual Valid("SELECT i_field, j_field, k_field FROM a_table")
        }
        "random fields" in {
          select[CTable].from[ATable].apply() mustEqual Valid("SELECT i_field, l_field, o_field, p_field FROM a_table")
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].apply() mustEqual Valid(
            "SELECT l_field, i_field, j_field, k_field, m_field FROM a_table"
          )
        }
        "two level nesting" in {
          select[ETable].from[ATable].apply() mustEqual Valid(
            "SELECT n_field, l_field, i_field, j_field, k_field, m_field, o_field FROM a_table"
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
          delete[ATable].apply() mustEqual Valid("DELETE FROM a_table")
        }
        "with single condition" in {
          delete[ATable]
            .where(f => f.iField == 1)
            .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1)")
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1) AND (j_field == TRUE)")
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            delete[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1) OR (j_field == TRUE)")
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM a_table WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "DELETE FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) OR (k_field LIKE '%foo%')"
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
          "INSERT INTO a_table (i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        )
      }
      "insert all fields from nested product" in {
        "one level nesting" in {
          insert[ATable].values[DTable].apply() mustEqual Valid(
            "INSERT INTO a_table (l_field, i_field, j_field, k_field, m_field) VALUES (?, ?, ?, ?, ?)"
          )
        }
        "two level nesting" in {
          insert[ATable].values[ETable].apply() mustEqual Valid(
            "INSERT INTO a_table (n_field, l_field, i_field, j_field, k_field, m_field, o_field) VALUES (?, ?, ?, ?, ?, ?, ?)"
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
            "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ?"
          )
        }

        "with single condition" in {
          update[ATable]
            .set[ATable]
            .where(f => f.iField == 1)
            .apply() mustEqual Valid(
            "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true)
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) AND (j_field == TRUE)"
            )
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true && f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 || f.jField == true)
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) OR (j_field == TRUE)"
            )
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 || f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(f => f.iField == 1 && f.jField == true || f.kField.like("%foo%"))
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) AND (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
        }
      }

      "update subset query" in {
        "first three fields" in {
          update[ATable].set[BTable].apply() mustEqual Valid("UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?")
        }
        "random fields" in {
          update[ATable].set[CTable].apply() mustEqual Valid(
            "UPDATE a_table SET i_field = ?, l_field = ?, o_field = ?, p_field = ?"
          )
        }
      }

      "update from nested product" in {
        "one level nesting" in {
          update[ATable].set[DTable].apply() mustEqual Valid(
            "UPDATE a_table SET l_field = ?, i_field = ?, j_field = ?, k_field = ?, m_field = ?"
          )
        }
        "two level nesting" in {
          update[ATable].set[ETable].apply() mustEqual Valid(
            "UPDATE a_table SET n_field = ?, l_field = ?, i_field = ?, j_field = ?, k_field = ?, m_field = ?, o_field = ?"
          )
        }
      }

    }
    "be invalid" in {
      update[ATable].apply() mustEqual Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")
    }
  }
}
