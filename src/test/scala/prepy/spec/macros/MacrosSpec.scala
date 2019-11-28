package prepy.spec.macros

import org.specs2.mutable._
import prepy.TestDomain
import prepy.operators._

class MacrosSpec extends Specification with TestDomain with Syntax {
  "single operators in expression" should {
    "match the equal check for an int field" in {
      stringify[ATable](f => f.iField == 1) mustEqual "iField = 1"
    }
    "match the equal check for a boolean field" in {
      stringify[ATable](f => f.jField == true) mustEqual "jField = TRUE"
    }
    "match the equal check for a char field" in {
      stringify[ATable](f => f.lField == 'c') mustEqual "lField = 'c'"
    }
    "match the equal check for a string field" in {
      stringify[ATable](f => f.kField == "abc") mustEqual "kField = 'abc'"
    }

    "match the not equal check for an int field" in {
      stringify[ATable](f => f.iField != 1) mustEqual "iField <> 1"
    }
    "match the not equal check for a boolean field" in {
      stringify[ATable](f => f.jField != true) mustEqual "jField <> TRUE"
    }
    "match the not equal check for a char field" in {
      stringify[ATable](f => f.lField != 'c') mustEqual "lField <> 'c'"
    }
    "match the not equal check for a string field" in {
      stringify[ATable](f => f.kField != "abc") mustEqual "kField <> 'abc'"
    }

    "match the greater then check for an int field" in {
      stringify[ATable](f => f.iField > 1) mustEqual "iField > 1"
    }
    "match the less then check for an int field" in {
      stringify[ATable](f => f.iField < 1) mustEqual "iField < 1"
    }
    "match the greater then or equal check for an int field" in {
      stringify[ATable](f => f.iField >= 1) mustEqual "iField >= 1"
    }
    "match the less then or equal check for an int field" in {
      stringify[ATable](f => f.iField <= 1) mustEqual "iField <= 1"
    }

    "match the add check for an int field" in {
      stringify[ATable](f => f.iField + 1 == 2) mustEqual "iField + 1 = 2"
    }
    "match the subtract check for an int field" in {
      stringify[ATable](f => f.iField - 1 == 0) mustEqual "iField - 1 = 0"
    }
    "match the multiply check for an int field" in {
      stringify[ATable](f => f.iField * 1 == f.iField) mustEqual "iField * 1 = iField"
    }
    "match the divide check for an int field" in {
      stringify[ATable](f => f.iField / 1 == f.iField) mustEqual "iField / 1 = iField"
    }
    "match the modulo check for an int field" in {
      stringify[ATable](f => f.iField % 1 == 0) mustEqual "iField MOD 1 = 0"
    }

    "match the and check for an boolean field" in {
      stringify[ATable](f => f.jField && true) mustEqual "(jField AND TRUE)"
    }
    "match the or check for an boolean field" in {
      stringify[ATable](f => f.jField || false) mustEqual "(jField OR FALSE)"
    }

    "match the in check for an int field" in {
      stringify[ATable](f => f.iField in List(1, 2, 3)) mustEqual "iField IN (1,2,3)"
    }
    "match the in check for a boolean field" in {
      stringify[ATable](f => f.jField in List(true, false)) mustEqual "jField IN (TRUE,FALSE)"
    }
    "match the in check for a char field" in {
      stringify[ATable](f => f.lField in List('c', 'd')) mustEqual "lField IN ('c','d')"
    }
  }
}
