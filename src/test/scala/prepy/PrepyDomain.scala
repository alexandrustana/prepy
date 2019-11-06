package prepy

import prepy.syntax._
import shapeless.cachedImplicit

trait PrepyDomain {
  case class ATable(
    iField: Int,
    jField: Boolean,
    kField: String,
    lField: Char,
    mField: Double,
    nField: Double,
    oField: List[Int],
    pField: Option[Float]
  )
  case class BTable(iField: Int, jField:    Boolean, kField: String)
  case class CTable(iField: Int, lField:    Char, oField:    List[Int], pField: Option[Float])
  case class DTable(lField: Char, bField:   BTable, mField:  Double)
  case class ETable(nField: Double, eField: DTable, oField:  List[Int])

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
