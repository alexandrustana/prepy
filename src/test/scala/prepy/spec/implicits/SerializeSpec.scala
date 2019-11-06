package prepy.spec.implicits

import prepy.syntax._
import org.specs2.mutable._
import prepy.TestDomain
import prepy.implicits.Implicits.Serialize
import shapeless.test.illTyped

class SerializeSpec extends Specification with TestDomain {

  "serialize" should {
    "have produce the same fields" in {
      "flat case class" in {
        implicitly[Serialize[ATable]].fields.map(_.name) === List(
          "iField",
          "jField",
          "kField",
          "lField",
          "mField",
          "nField",
          "oField",
          "pField"
        )
      }

      "one level nested case class" in {
        implicitly[Serialize[DTable]].fields.map(_.name) === List(
          "lField",
          "iField",
          "jField",
          "kField",
          "mField"
        )
      }

      "two levels nested case class" in {
        implicitly[Serialize[ETable]].fields.map(_.name) === List(
          "nField",
          "lField",
          "iField",
          "jField",
          "kField",
          "mField",
          "oField"
        )
      }
    }
  }
}
