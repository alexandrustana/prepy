package prepy.syntax.factory.doobie

import cats.data.Validated
import cats.data.Validated.Valid
import doobie.util.Read
import doobie.util.query.Query0
import prepy.syntax.factory.SelectFactory
import prepy.syntax.factory.plain.PlainSelect
import prepy.syntax.ast.{QueryElement, SelectSyntax}

trait DoobieSelect extends PlainSelect {

  override def `fromT`[T <: Product: Read](
    elem:      QueryElement,
    tableName: String,
    factory:   SelectFactory
  ): SelectSyntax.`fromT`[T] =
    new SelectSyntax.`fromT`[T](elem, tableName, factory) {
      override def apply(): Validated[String, _] = Valid(Query0[T](toString))
    }
}
