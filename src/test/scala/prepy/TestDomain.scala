package prepy

trait TestDomain {
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

}
