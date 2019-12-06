package prepy.spec.syntax

import prepy.syntax._
import prepy.TestDomain
import prepy.syntax.implicits.Implicits._
import shapeless.cachedImplicit

trait TestImplicits extends TestDomain {
  implicit val aDomain: Serialize[ATable] = cachedImplicit[Serialize[ATable]]
  implicit val bDomain: Serialize[BTable] = cachedImplicit[Serialize[BTable]]
  implicit val cDomain: Serialize[CTable] = cachedImplicit[Serialize[CTable]]
  implicit val dDomain: Serialize[DTable] = cachedImplicit[Serialize[DTable]]
  implicit val EDomain: Serialize[ETable] = cachedImplicit[Serialize[ETable]]

  implicit val AtoA: Transform[ATable, ATable] = cachedImplicit[Transform[ATable, ATable]]
  implicit val AtoB: Transform[ATable, BTable] = cachedImplicit[Transform[ATable, BTable]]
  implicit val AtoC: Transform[ATable, CTable] = cachedImplicit[Transform[ATable, CTable]]
  implicit val AtoD: Transform[ATable, DTable] = cachedImplicit[Transform[ATable, DTable]]
  implicit val AtoE: Transform[ATable, ETable] = cachedImplicit[Transform[ATable, ETable]]
}
