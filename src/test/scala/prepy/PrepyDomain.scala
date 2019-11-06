package prepy

import prepy.syntax._
import shapeless.cachedImplicit

trait PrepyDomain {
  case class ATable(i: Int, j:    Boolean, k: String, l: Char, m: Double, n: Double, o: List[Int], p: Option[Float])
  case class BTable(i: Int, j:    Boolean, k: String)
  case class CTable(i: Int, l:    Char, o:    List[Int], p: Option[Float])
  case class DTable(l: Char, b:   BTable, m:  Double)
  case class ETable(n: Double, e: DTable, o:  List[Int])

  implicit val aDomain: Domain[ATable] = cachedImplicit[Domain[ATable]]
  implicit val bDomain: Domain[BTable] = cachedImplicit[Domain[BTable]]
  implicit val cDomain: Domain[CTable] = cachedImplicit[Domain[CTable]]
  implicit val dDomain: Domain[DTable] = cachedImplicit[Domain[DTable]]
  implicit val EDomain: Domain[ETable] = cachedImplicit[Domain[ETable]]

  implicit val AtoA: Transform[ATable, ATable] = cachedImplicit[Transform[ATable, ATable]]
  implicit val AtoB: Transform[ATable, BTable] = cachedImplicit[Transform[ATable, BTable]]
  implicit val AtoC: Transform[ATable, CTable] = cachedImplicit[Transform[ATable, CTable]]
  implicit val AtoD: Transform[ATable, DTable] = cachedImplicit[Transform[ATable, DTable]]
  implicit val AtoE: Transform[ATable, ETable] = cachedImplicit[Transform[ATable, ETable]]
}
