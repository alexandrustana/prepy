package prepy

import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.syntax._
import prepy.formatter.postgresql._
import shapeless.cachedImplicit

class PostgresqlFormatterSpec extends Specification {

  case class ATable(
    iField: Int,
    jField: Boolean,
    kField: String,
    lField: Char,
    mField: Double,
    nField: Double,
    oField: List[Int],
    pField: Option[Float]
  )
  case class BTable(iField: Int, jField: Boolean, kField: String)
  case class CTable(iField: Int, lField: Char, oField:    List[Int], pField: Option[Float])
  case class DTable(aField: Int, bField: BTable, cField:  String)
  case class ETable(dField: Int, eField: DTable, fField:  String)

  implicit val aDomain = cachedImplicit[Domain[ATable]]
  implicit val bDomain = cachedImplicit[Domain[BTable]]
  implicit val cDomain = cachedImplicit[Domain[CTable]]
  implicit val dDomain = cachedImplicit[Domain[DTable]]
  implicit val EDomain = cachedImplicit[Domain[ETable]]

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
            .where("i_field == 1")
            .apply() mustEqual Valid(
            "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) AND (j_field == TRUE)"
            )
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .and("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) OR (j_field == TRUE)"
            )
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "SELECT i_field, j_field, k_field, l_field, m_field, n_field, o_field, p_field FROM a_table WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
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
            "SELECT a_field, i_field, j_field, k_field, c_field FROM a_table"
          )
        }
        "two level nesting" in {
          select[ETable].from[ATable].apply() mustEqual Valid(
            "SELECT d_field, a_field, i_field, j_field, k_field, c_field, f_field FROM a_table"
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
            .where("i_field == 1")
            .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1)")
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1) AND (j_field == TRUE)")
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .and("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "DELETE FROM a_table WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            delete[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .apply() mustEqual Valid("DELETE FROM a_table WHERE (i_field == 1) OR (j_field == TRUE)")
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "DELETE FROM a_table WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
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
          insert[DTable].values[DTable].apply() mustEqual Valid(
            "INSERT INTO d_table (a_field, i_field, j_field, k_field, c_field) VALUES (?, ?, ?, ?, ?)"
          )
        }
        "two level nesting" in {
          insert[ETable].values[ETable].apply() mustEqual Valid(
            "INSERT INTO e_table (d_field, a_field, i_field, j_field, k_field, c_field, f_field) VALUES (?, ?, ?, ?, ?, ?, ?)"
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
            .where("i_field == 1")
            .apply() mustEqual Valid(
            "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1)"
          )
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) AND (j_field == TRUE)"
            )
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .and("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) AND (j_field == TRUE) AND (k_field LIKE '%foo%')"
            )
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) OR (j_field == TRUE)"
            )
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where("i_field == 1")
              .or("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
              .apply() mustEqual Valid(
              "UPDATE a_table SET i_field = ?, j_field = ?, k_field = ?, l_field = ?, m_field = ?, n_field = ?, o_field = ?, p_field = ? WHERE (i_field == 1) OR (j_field == TRUE) OR (k_field LIKE '%foo%')"
            )
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where("i_field == 1")
              .and("j_field == TRUE")
              .or("k_field LIKE '%foo%'")
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
            "UPDATE a_table SET a_field = ?, i_field = ?, j_field = ?, k_field = ?, c_field = ?"
          )
        }
        "two level nesting" in {
          update[ATable].set[ETable].apply() mustEqual Valid(
            "UPDATE a_table SET d_field = ?, a_field = ?, i_field = ?, j_field = ?, k_field = ?, c_field = ?, f_field = ?"
          )
        }
      }

    }
    "be invalid" in {
      update[ATable].apply() mustEqual Invalid("Incomplete SQL query. `update[T]` must be followed by a `set[K]`")
    }
  }
}
