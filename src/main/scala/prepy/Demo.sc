import prepy.syntax._
import prepy.implicits.Implicits.Transform


case class ATable(i: Int, j: Boolean, k: String, l: Char, m: Double, n: Double, o: List[Int], p: Option[Float])
case class BTable(i: Int, j: Boolean, k: String)
case class CTable(i: Int, l: Char, o:    List[Int], p: Option[Float])
case class DTable(a: Int, b: BTable, c:  String)
case class ETable(d: Int, e: DTable, f:  String)

Transform[ATable, CTable]