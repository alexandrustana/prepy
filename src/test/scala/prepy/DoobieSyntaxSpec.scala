package prepy

import doobie.util.Meta
import doobie.implicits._
import org.specs2.mutable._
import prepy.syntax.doobie._
import shapeless.cachedImplicit

class DoobieSyntaxSpec extends Specification {

  case class ATable(i: Int, j: Boolean, k: String, l: Char, m: Double, n: Double, o: List[Int], p: Option[Float])

  case class BTable(i: Int, j: Boolean, k: String)

  case class CTable(i: Int, l: Char, o: List[Int], p: Option[Float])

  case class DTable(a: Int, b: BTable, c: String)

  case class ETable(d: Int, e: DTable, f: String)

  implicit val aDomain = cachedImplicit[Domain[ATable]]
  implicit val bDomain = cachedImplicit[Domain[BTable]]
  implicit val cDomain = cachedImplicit[Domain[CTable]]
  implicit val dDomain = cachedImplicit[Domain[DTable]]
  implicit val EDomain = cachedImplicit[Domain[ETable]]

  implicit val intListMeta: Meta[List[Int]] =
    Meta[String].timap(arr => arr.split(",").map(_.toInt).toList)(_.mkString(","))
  implicit val optionFloatMeta: Meta[Char] = Meta[String].timap(_.charAt(0))(_.toString)

  "select" should {

    "be equal" in {
      "select query" in {
        "without condition" in {
          select[ATable].from[ATable].query().sql.trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable"
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where(fr0"i == 1")
            .query()
            .sql
            .trim
            .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .query()
              .sql
              .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .and(fr0"k LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .query()
              .sql
              .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }

        "with interpolated condition" in {
          val i = 1;
          "single condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"i == $i")
              .query()
              .sql
              .trim
              .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?)"
          }

          "multiple conditions" in {
            val j = true
            "single AND condition" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .query()
                .sql
                .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) AND (j == ?)"
            }
            val k = "foo"
            "multiple AND conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .and(fr0"k LIKE '%$k%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) AND (j == ?) AND (k LIKE '%?%')"
            }
            "single OR condition" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .query()
                .sql
                .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) OR (j == ?)"
            }
            "multiple OR conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) OR (j == ?) OR (k LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) AND (j == ?) OR (k LIKE '%?%')"
            }
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].query().sql.trim mustEqual "SELECT i, j, k FROM ATable"
        }
        "random fields" in {
          select[CTable].from[ATable].query().sql.trim mustEqual "SELECT i, l, o, p FROM ATable"
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].query().sql.trim mustEqual "SELECT a, i, j, k, c FROM ATable"
        }
        "two level nesting" in {
          select[ETable].from[ATable].query().sql.trim mustEqual "SELECT d, a, i, j, k, c, f FROM ATable"
        }
      }

    }
  }

  "delete" should {
    "be equal" in {
      "delete from" in {
        "without condition" in {
          delete[ATable].update().sql.trim mustEqual "DELETE FROM ATable"
        }
        "with single condition" in {
          delete[ATable]
            .where("i == 1")
            .update()
            .sql
            .trim mustEqual "DELETE FROM ATable WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }
      }
    }
  }

  "insert" should {
    "be equal" in {
      "insert all fields" in {
        insert[ATable].values[ATable].update().sql.trim mustEqual
          "INSERT INTO ATable (i, j, k, l, m, n, o, p) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      }
      "insert all fields from nested product" in {
        "one level nesting" in {
          insert[DTable].values[DTable].update().sql.trim mustEqual
            "INSERT INTO DTable (a, i, j, k, c) VALUES (?, ?, ?, ?, ?)"
        }
        "two level nesting" in {
          insert[ETable].values[ETable].update().sql.trim mustEqual
            "INSERT INTO ETable (d, a, i, j, k, c, f) VALUES (?, ?, ?, ?, ?, ?, ?)"
        }
      }
    }
  }

  "update" should {

    "be equal" in {
      "update all query" in {
        "without condition" in {
          update[ATable].set[ATable].update().sql.trim mustEqual
            "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ?"
        }

        "with single condition" in {
          update[ATable]
            .set[ATable]
            .where(fr0"i == 1")
            .update()
            .sql
            .trim mustEqual
            "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .and(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }
      }

      "update subset query" in {
        "first three fields" in {
          update[ATable].set[BTable].update().sql.trim mustEqual "UPDATE ATable SET i = ?, j = ?, k = ?"
        }
        "random fields" in {
          update[ATable].set[CTable].update().sql.trim mustEqual "UPDATE ATable SET i = ?, l = ?, o = ?, p = ?"
        }
      }

      "update from nested product" in {
        "one level nesting" in {
          update[ATable].set[DTable].update().sql.trim mustEqual "UPDATE ATable SET a = ?, i = ?, j = ?, k = ?, c = ?"
        }
        "two level nesting" in {
          update[ATable].set[ETable].update().sql.trim mustEqual
            "UPDATE ATable SET d = ?, a = ?, i = ?, j = ?, k = ?, c = ?, f = ?"
        }
      }

    }
  }
}
