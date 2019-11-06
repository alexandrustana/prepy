package prepy.spec.syntax

import doobie.implicits._
import doobie.util.Meta
import org.specs2.mutable._
import prepy.PrepyDomain
import prepy.syntax.doobie._

class DoobieSyntaxSpec extends Specification with PrepyDomain {

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

        "with interpolated conditions" in {
          val i = 1
          val j = true
          val k = "foo"
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
            "single AND condition" in {
              select[ATable]
                .from[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .query()
                .sql
                .trim mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == ?) AND (j == ?)"
            }
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
          select[DTable].from[ATable].query().sql.trim mustEqual "SELECT l, i, j, k, m FROM ATable"
        }
        "two level nesting" in {
          select[ETable].from[ATable].query().sql.trim mustEqual "SELECT n, l, i, j, k, m, o FROM ATable"
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
            .where(fr0"i == 1")
            .update()
            .sql
            .trim mustEqual "DELETE FROM ATable WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .and(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            delete[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where(fr0"i == 1")
              .or(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where(fr0"i == 1")
              .and(fr0"j == TRUE")
              .or(fr0"k LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }

        "with interpolated conditions" in {
          val i = 1
          val j = true
          val k = "foo"
          "with single condition" in {
            delete[ATable]
              .where(fr0"i == $i")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (i == ?)"
          }

          "with multiple conditions" in {
            "single AND condition" in {
              delete[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .update()
                .sql
                .trim mustEqual "DELETE FROM ATable WHERE (i == ?) AND (j == ?)"
            }
            "multiple AND conditions" in {
              delete[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .and(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (i == ?) AND (j == ?) AND (k LIKE '%?%')"
            }
            "single OR condition" in {
              delete[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .update()
                .sql
                .trim mustEqual "DELETE FROM ATable WHERE (i == ?) OR (j == ?)"
            }
            "multiple OR conditions" in {
              delete[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (i == ?) OR (j == ?) OR (k LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              delete[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (i == ?) AND (j == ?) OR (k LIKE '%?%')"
            }
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
            "INSERT INTO DTable (l, i, j, k, m) VALUES (?, ?, ?, ?, ?)"
        }
        "two level nesting" in {
          insert[ETable].values[ETable].update().sql.trim mustEqual
            "INSERT INTO ETable (n, l, i, j, k, m, o) VALUES (?, ?, ?, ?, ?, ?, ?)"
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

        "with interpolated conditions" in {
          val i = 1
          val j = true
          val k = "foo"
          "single condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"i == $i")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?)"
          }

          "multiple conditions" in {
            "single AND condition" in {
              update[ATable]
                .set[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?) AND (j == ?)"
            }
            "multiple AND conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .and(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?) AND (j == ?) AND (k LIKE '%?%')"
            }
            "single OR condition" in {
              update[ATable]
                .set[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?) OR (j == ?)"
            }
            "multiple OR conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"i == $i")
                .or(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?) OR (j == ?) OR (k LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"i == $i")
                .and(fr0"j == $j")
                .or(fr0"k LIKE '%$k%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET i = ?, j = ?, k = ?, l = ?, m = ?, n = ?, o = ?, p = ? WHERE (i == ?) AND (j == ?) OR (k LIKE '%?%')"
            }
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
          update[ATable].set[DTable].update().sql.trim mustEqual "UPDATE ATable SET l = ?, i = ?, j = ?, k = ?, m = ?"
        }
        "two level nesting" in {
          update[ATable].set[ETable].update().sql.trim mustEqual
            "UPDATE ATable SET n = ?, l = ?, i = ?, j = ?, k = ?, m = ?, o = ?"
        }
      }

    }
  }
}
