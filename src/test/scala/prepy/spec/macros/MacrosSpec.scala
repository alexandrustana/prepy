package prepy.spec.macros

import org.specs2.mutable._
import prepy.TestDomain
import prepy.operators._

class MacrosSpec extends Specification with TestDomain with Syntax {
  "single operators in expression" should {
    "match ==" should {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField == 1) mustEqual "iField = 1"
      }
      "be equal for a boolean field" in {
        stringify[ATable](f => f.jField == true) mustEqual "jField = TRUE"
      }
      "be equal for a char field" in {
        stringify[ATable](f => f.lField == 'c') mustEqual "lField = 'c'"
      }
      "be equal for a string field" in {
        stringify[ATable](f => f.kField == "abc") mustEqual "kField = 'abc'"
      }
    }

    "match !=" in {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField != 1) mustEqual "iField <> 1"
      }
      "be equal for a boolean field" in {
        stringify[ATable](f => f.jField != true) mustEqual "jField <> TRUE"
      }
      "be equal for a char field" in {
        stringify[ATable](f => f.lField != 'c') mustEqual "lField <> 'c'"
      }
      "be equal for a string field" in {
        stringify[ATable](f => f.kField != "abc") mustEqual "kField <> 'abc'"
      }
    }

    "match > and >=" should {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField > 1) mustEqual "iField > 1"
      }
      "be equal for an int field" in {
        stringify[ATable](f => f.iField >= 1) mustEqual "iField >= 1"
      }
    }

    "match < and <=" should {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField < 1) mustEqual "iField < 1"
      }
      "be equal for an int field" in {
        stringify[ATable](f => f.iField <= 1) mustEqual "iField <= 1"
      }
    }

    "match mathematical operators" should {
      "be equal for an int field add operation" in {
        stringify[ATable](f => f.iField + 1 == 2) mustEqual "iField + 1 = 2"
      }
      "be equal for an int field subtract operation" in {
        stringify[ATable](f => f.iField - 1 == 0) mustEqual "iField - 1 = 0"
      }
      "be equal for an int field multiply operation" in {
        stringify[ATable](f => f.iField * 1 == f.iField) mustEqual "iField * 1 = iField"
      }
      "be equal for an int field division operation" in {
        stringify[ATable](f => f.iField / 1 == f.iField) mustEqual "iField / 1 = iField"
      }
      "be equal for an int field modulo operation" in {
        stringify[ATable](f => f.iField % 1 == 0) mustEqual "iField MOD 1 = 0"
      }
    }

    "match logical operators" should {
      "be equal for an boolean field and operation" in {
        stringify[ATable](f => f.jField && true) mustEqual "(jField AND TRUE)"
      }
      "be equal for an boolean field or operation" in {
        stringify[ATable](f => f.jField || false) mustEqual "(jField OR FALSE)"
      }
    }

    "match in" should {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField in List(1, 2, 3)) mustEqual "iField IN (1,2,3)"
      }
      "be equal for a boolean field" in {
        stringify[ATable](f => f.jField in List(true, false)) mustEqual "jField IN (TRUE,FALSE)"
      }
      "be equal for a char field" in {
        stringify[ATable](f => f.lField in List('c', 'd')) mustEqual "lField IN ('c','d')"
      }
    }

    "match like" should {
      "be equal for an string field without wildcard" in {
        stringify[ATable](f => f.kField.like("abc")) mustEqual "kField LIKE 'abc'"
      }
      "be equal for an string field with a left wildcard" in {
        stringify[ATable](f => f.kField.like("%abc")) mustEqual "kField LIKE '%abc'"
      }
      "be equal for an string field with a right wildcard" in {
        stringify[ATable](f => f.kField.like("abc%")) mustEqual "kField LIKE 'abc%'"
      }
      "be equal for an string field with a left and right wildcards" in {
        stringify[ATable](f => f.kField.like("%abc%")) mustEqual "kField LIKE '%abc%'"
      }
    }

    "match between" should {
      "be equal for an int field" in {
        stringify[ATable](f => f.iField between 1 and 2) mustEqual "iField BETWEEN 1 AND 2"
      }
      "be equal for an char field" in {
        stringify[ATable](f => f.lField between 'c' and 'd') mustEqual "lField BETWEEN 'c' AND 'd'"
      }
    }

  }
}
