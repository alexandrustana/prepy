package prepy.spec.implicits

import org.specs2.mutable._
import prepy.TestDomain
import prepy.syntax.implicits.Internal.Validate
import shapeless.test.illTyped

class ValidateSpec extends Specification with TestDomain {

  "transform" should {
    "succeed" in {
      "same case class" in {
        implicitly[Validate[ATable, ATable]]
        success
      }

      "different case class" in {
        "flat case class" in {
          implicitly[Validate[ATable, BTable]]
          success
        }

        "one level nested case class" in {
          implicitly[Validate[ATable, DTable]]
          success
        }

        "two levels nested case class" in {
          implicitly[Validate[ATable, ETable]]
          success
        }
      }

      "dummy validation" in {
        implicit val dummy: Validate[ETable, ATable] = Validate.dummy[ETable, ATable]
        implicitly[Validate[ETable, ATable]]
        success
      }
    }

    "fail" in {
      "different fields" in {
        case class Foo(i: Int, j:     String, k: Double)
        case class Boo(a: Boolean, b: Int, c:    String)
        illTyped("implicitly[Transform[Foo, Boo]]")
        success
      }

      "different types" in {
        case class Foo(i: Int, j: String, k:  Double)
        case class Boo(i: Int, j: Boolean, k: Double)
        illTyped("implicitly[Transform[Foo, Boo]]")
        success
      }

      "bigger case classes" in {
        case class Foo(i: Int, j: String, k: Double)
        case class Boo(i: Int, j: String, k: Double, l: Char)
        illTyped("implicitly[Transform[Foo, Boo]]")
        success
      }
    }
  }
}
