package prepy.spec.implicits

import prepy.syntax._
import org.specs2.mutable._
import prepy.TestDomain
import prepy.syntax.implicits.Internal.Transform
import shapeless.test.illTyped

class TransformSpec extends Specification with TestDomain {

  "transform" should {
    "succeed" in {
      "same case class" in {
        implicitly[Transform[ATable, ATable]]
        success
      }

      "different case class" in {
        "flat case class" in {
          implicitly[Transform[ATable, BTable]]
          success
        }

        "one level nested case class" in {
          implicitly[Transform[ATable, DTable]]
          success
        }

        "two levels nested case class" in {
          implicitly[Transform[ATable, ETable]]
          success
        }
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
