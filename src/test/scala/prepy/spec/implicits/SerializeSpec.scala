package prepy.spec.implicits

import prepy.syntax._
import org.specs2.mutable._
import prepy.TestDomain
import prepy.syntax.implicits.Implicits.Serialize
import shapeless.test.illTyped

class SerializeSpec extends Specification with TestDomain {

  "serialize" should {
    "produce the same fields" in {
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

    "fail" in {
      "using a class" in {
        class Foo(i: Int, j: String, k: Double)

        illTyped("implicitly[Serialize[Foo]]")
        success
      }

      "using a trait" in {
        trait Foo

        illTyped("implicitly[Serialize[Foo]]")
        success
      }
    }
  }
}
