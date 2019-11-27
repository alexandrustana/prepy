package prepy.spec.macros

import org.specs2.mutable._
import prepy.TestDomain
import prepy.operators._
import shapeless.test.illTyped

class MacrosSpec extends Specification with TestDomain with Syntax {
  "stringify" should {
//    "match the equality check for a int field" in {
//      stringify[ATable](f => f.iField == 1) mustEqual "iField = 1"
//    }
//    "match the equality check for a boolean field" in {
//      stringify[ATable](f => f.jField == true) mustEqual "jField = true"
//    }
//    "match the equality check for a char field" in {
//      stringify[ATable](f => f.lField == 'c') mustEqual "lField = 'c'"
//    }
//    "match the equality check for a string field" in {
//      stringify[ATable](f => f.kField == "abc" && f.iField == 1) mustEqual "(kField = 'abc' AND iField = 1)"
//    }
//    "fail when trying to check the equality for not matching types" in {
//      illTyped("stringify[ATable](f => f.kField == true)")
//      success
//    }

    "in test" in {
      stringify[ATable](f => f.iField in List(1, 2, 3)) mustEqual "iField IN (1,2,3)"
    }
  }
}
