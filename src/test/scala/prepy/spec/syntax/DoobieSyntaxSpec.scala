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
          select[ATable].from[ATable].query().sql.trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable"
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where(fr0"iField == 1")
            .query()
            .sql
            .trim
            .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .query()
              .sql
              .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE)"
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .and(fr0"kField LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .query()
              .sql
              .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) OR (jField == TRUE)"
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .query()
              .sql
              .trim mustEqual
              "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
          }
        }

        "with interpolated conditions" in {
          val iField = 1
          val jField = true
          val kField = "foo"
          "single condition" in {
            select[ATable]
              .from[ATable]
              .where(fr0"iField == $iField")
              .query()
              .sql
              .trim
              .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?)"
          }

          "multiple conditions" in {
            "single AND condition" in {
              select[ATable]
                .from[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .query()
                .sql
                .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?) AND (jField == ?)"
            }
            "multiple AND conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .and(fr0"kField LIKE '%$kField%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?) AND (jField == ?) AND (kField LIKE '%?%')"
            }
            "single OR condition" in {
              select[ATable]
                .from[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .query()
                .sql
                .trim mustEqual "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?) OR (jField == ?)"
            }
            "multiple OR conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?) OR (jField == ?) OR (kField LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              select[ATable]
                .from[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .query()
                .sql
                .trim mustEqual
                "SELECT iField, jField, kField, lField, mField, nField, oField, pField FROM ATable WHERE (iField == ?) AND (jField == ?) OR (kField LIKE '%?%')"
            }
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].query().sql.trim mustEqual "SELECT iField, jField, kField FROM ATable"
        }
        "random fields" in {
          select[CTable].from[ATable].query().sql.trim mustEqual "SELECT iField, lField, oField, pField FROM ATable"
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].query().sql.trim mustEqual "SELECT lField, iField, jField, kField, mField FROM ATable"
        }
        "two level nesting" in {
          select[ETable].from[ATable].query().sql.trim mustEqual "SELECT nField, lField, iField, jField, kField, mField, oField FROM ATable"
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
            .where(fr0"iField == 1")
            .update()
            .sql
            .trim mustEqual "DELETE FROM ATable WHERE (iField == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE)"
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .and(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
          }
          "single OR condition" in {
            delete[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (iField == 1) OR (jField == TRUE)"
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "DELETE FROM ATable WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
          }
        }

        "with interpolated conditions" in {
          val iField = 1
          val jField = true
          val kField = "foo"
          "with single condition" in {
            delete[ATable]
              .where(fr0"iField == $iField")
              .update()
              .sql
              .trim mustEqual "DELETE FROM ATable WHERE (iField == ?)"
          }

          "with multiple conditions" in {
            "single AND condition" in {
              delete[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .update()
                .sql
                .trim mustEqual "DELETE FROM ATable WHERE (iField == ?) AND (jField == ?)"
            }
            "multiple AND conditions" in {
              delete[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .and(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (iField == ?) AND (jField == ?) AND (kField LIKE '%?%')"
            }
            "single OR condition" in {
              delete[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .update()
                .sql
                .trim mustEqual "DELETE FROM ATable WHERE (iField == ?) OR (jField == ?)"
            }
            "multiple OR conditions" in {
              delete[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (iField == ?) OR (jField == ?) OR (kField LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              delete[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "DELETE FROM ATable WHERE (iField == ?) AND (jField == ?) OR (kField LIKE '%?%')"
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
          "INSERT INTO ATable (iField, jField, kField, lField, mField, nField, oField, pField) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      }
      "insert all fields from nested product" in {
        "one level nesting" in {
          insert[ATable].values[DTable].update().sql.trim mustEqual
            "INSERT INTO ATable (lField, iField, jField, kField, mField) VALUES (?, ?, ?, ?, ?)"
        }
        "two level nesting" in {
          insert[ATable].values[ETable].update().sql.trim mustEqual
            "INSERT INTO ATable (nField, lField, iField, jField, kField, mField, oField) VALUES (?, ?, ?, ?, ?, ?, ?)"
        }
      }
    }
  }

  "update" should {

    "be equal" in {
      "update all query" in {
        "without condition" in {
          update[ATable].set[ATable].update().sql.trim mustEqual
            "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ?"
        }

        "with single condition" in {
          update[ATable]
            .set[ATable]
            .where(fr0"iField == 1")
            .update()
            .sql
            .trim mustEqual
            "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE)"
          }
          "multiple AND conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .and(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE) AND (kField LIKE '%foo%')"
          }
          "single OR condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) OR (jField == TRUE)"
          }
          "multiple OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == 1")
              .or(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) OR (jField == TRUE) OR (kField LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == 1")
              .and(fr0"jField == TRUE")
              .or(fr0"kField LIKE '%foo%'")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == 1) AND (jField == TRUE) OR (kField LIKE '%foo%')"
          }
        }

        "with interpolated conditions" in {
          val iField = 1
          val jField = true
          val kField = "foo"
          "single condition" in {
            update[ATable]
              .set[ATable]
              .where(fr0"iField == $iField")
              .update()
              .sql
              .trim mustEqual
              "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?)"
          }

          "multiple conditions" in {
            "single AND condition" in {
              update[ATable]
                .set[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?) AND (jField == ?)"
            }
            "multiple AND conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .and(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?) AND (jField == ?) AND (kField LIKE '%?%')"
            }
            "single OR condition" in {
              update[ATable]
                .set[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?) OR (jField == ?)"
            }
            "multiple OR conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"iField == $iField")
                .or(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?) OR (jField == ?) OR (kField LIKE '%?%')"
            }
            "mixed AND with OR conditions" in {
              update[ATable]
                .set[ATable]
                .where(fr0"iField == $iField")
                .and(fr0"jField == $jField")
                .or(fr0"kField LIKE '%$kField%'")
                .update()
                .sql
                .trim mustEqual
                "UPDATE ATable SET iField = ?, jField = ?, kField = ?, lField = ?, mField = ?, nField = ?, oField = ?, pField = ? WHERE (iField == ?) AND (jField == ?) OR (kField LIKE '%?%')"
            }
          }
        }
      }

      "update subset query" in {
        "first three fields" in {
          update[ATable].set[BTable].update().sql.trim mustEqual "UPDATE ATable SET iField = ?, jField = ?, kField = ?"
        }
        "random fields" in {
          update[ATable].set[CTable].update().sql.trim mustEqual "UPDATE ATable SET iField = ?, lField = ?, oField = ?, pField = ?"
        }
      }

      "update from nested product" in {
        "one level nesting" in {
          update[ATable].set[DTable].update().sql.trim mustEqual "UPDATE ATable SET lField = ?, iField = ?, jField = ?, kField = ?, mField = ?"
        }
        "two level nesting" in {
          update[ATable].set[ETable].update().sql.trim mustEqual
            "UPDATE ATable SET nField = ?, lField = ?, iField = ?, jField = ?, kField = ?, mField = ?, oField = ?"
        }
      }

    }
  }
}
