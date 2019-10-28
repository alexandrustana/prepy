package prepy

import cats.data.Validated.{Invalid, Valid}
import org.specs2.mutable._
import prepy.syntax._
import prepy.implicits._
import prepy.syntax.factory.plain.PlainQuery
import shapeless.cachedImplicit
import prepy.syntax.factory.doobie.DoobieQuery._

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
  implicit val factory = PlainQuery

  /*_*/
  "select" should {

    "be equal" in {
      "select query" in {
        "without condition" in {
          select[ATable].from[ATable].query().sql mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable"
        }

        "with single condition" in {
          select[ATable]
            .from[ATable]
            .where("i == 1")
            .query()
            .sql mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .query()
              .sql mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .query()
              .sql mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .query()
              .sql mustEqual "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .query()
              .sql mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            select[ATable]
              .from[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .query()
              .sql mustEqual
              "SELECT i, j, k, l, m, n, o, p FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }
      }

      "select subset query" in {
        "first three fields" in {
          select[BTable].from[ATable].query().sql mustEqual "SELECT i, j, k FROM ATable"
        }
        "random fields" in {
          select[CTable].from[ATable].query().sql mustEqual "SELECT i, l, o, p FROM ATable"
        }
      }

      "select * from nested product" in {
        "one level nesting" in {
          select[DTable].from[ATable].query().sql mustEqual "SELECT a, i, j, k, c FROM ATable"
        }
        "two level nesting" in {
          select[ETable].from[ATable].query().sql mustEqual "SELECT d, a, i, j, k, c, f FROM ATable"
        }
      }

    }
  }

  "delete" should {
    "be equal" in {
      "delete from" in {
        "without condition" in {
          delete[ATable].update().sql mustEqual "DELETE FROM ATable"
        }
        "with single condition" in {
          delete[ATable]
            .where("i == 1")
            .update()
            .sql mustEqual "DELETE FROM ATable WHERE (i == 1)"
        }

        "with multiple conditions" in {
          "single AND condition" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .update()
              .sql mustEqual "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE)"
          }
          "multiple AND conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .and("k LIKE '%foo%'")
              .update()
              .sql mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) AND (k LIKE '%foo%')"
          }
          "single OR condition" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .update()
              .sql mustEqual "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE)"
          }
          "multiple OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .or("j == TRUE")
              .or("k LIKE '%foo%'")
              .update()
              .sql mustEqual
              "DELETE FROM ATable WHERE (i == 1) OR (j == TRUE) OR (k LIKE '%foo%')"
          }
          "mixed AND with OR conditions" in {
            delete[ATable]
              .where("i == 1")
              .and("j == TRUE")
              .or("k LIKE '%foo%'")
              .update()
              .sql mustEqual
              "DELETE FROM ATable WHERE (i == 1) AND (j == TRUE) OR (k LIKE '%foo%')"
          }
        }
      }
    }
  }
  /*_*/
}
