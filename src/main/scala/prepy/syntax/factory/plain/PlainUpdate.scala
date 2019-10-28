package prepy.syntax.factory.plain

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.syntax.factory.{SelectFactory, UpdateFactory}
import prepy.syntax.ast.{QueryElement, SelectSyntax, UpdateSyntax}

trait PlainUpdate extends UpdateFactory {
  override def `updateT`(tableName: String, factory: UpdateFactory): UpdateSyntax.`updateT` =
    new UpdateSyntax.`updateT`(tableName, factory) {}

  override def `setT`(elem: QueryElement, fields: List[Symbol], factory: UpdateFactory): UpdateSyntax.`setT` =
    new UpdateSyntax.`setT`(elem, fields, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `whereT`(elem: QueryElement, predicate: String, factory: UpdateFactory): UpdateSyntax.`whereT` =
    new UpdateSyntax.`whereT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `andT`(elem: QueryElement, predicate: String, factory: UpdateFactory): UpdateSyntax.`andT` =
    new UpdateSyntax.`andT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `orT`(elem: QueryElement, predicate: String, factory: UpdateFactory): UpdateSyntax.`orT` =
    new UpdateSyntax.`orT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }
}
