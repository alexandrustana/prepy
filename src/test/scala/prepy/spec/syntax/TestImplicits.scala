package prepy.spec.syntax

import prepy.TestDomain
import prepy.syntax.implicits.Internal._
import shapeless.cachedImplicit

trait TestImplicits extends TestDomain {
  implicit val AtransformA: Transform[ATable, ATable] = cachedImplicit[Transform[ATable, ATable]]
  implicit val AtransformB: Transform[ATable, BTable] = cachedImplicit[Transform[ATable, BTable]]
  implicit val AtransformC: Transform[ATable, CTable] = cachedImplicit[Transform[ATable, CTable]]
  implicit val AtransformD: Transform[ATable, DTable] = cachedImplicit[Transform[ATable, DTable]]
  implicit val AtransformE: Transform[ATable, ETable] = cachedImplicit[Transform[ATable, ETable]]
}
