package prepy.spec.syntax

import cats.MonadError
import cats.effect.IO
import prepy.TestDomain
import prepy.syntax.implicits.Internal._
import shapeless.cachedImplicit

trait TestImplicits extends TestDomain {
  implicit lazy val async: MonadError[IO, Throwable] = IO.ioEffect

  implicit val AtransformA: Transform[ATable, ATable] = cachedImplicit[Transform[ATable, ATable]]
  implicit val AtransformB: Transform[ATable, BTable] = cachedImplicit[Transform[ATable, BTable]]
  implicit val AtransformC: Transform[ATable, CTable] = cachedImplicit[Transform[ATable, CTable]]
  implicit val AtransformD: Transform[ATable, DTable] = cachedImplicit[Transform[ATable, DTable]]
  implicit val AtransformE: Transform[ATable, ETable] = cachedImplicit[Transform[ATable, ETable]]
}
