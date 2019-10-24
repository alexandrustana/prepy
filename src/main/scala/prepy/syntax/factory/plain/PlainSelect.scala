package prepy.syntax.factory.plain

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.syntax.factory.SelectFactory
import prepy.syntax.ast
import prepy.syntax.ast.{QueryElement, SelectSyntax}

trait PlainSelect extends SelectFactory {
  override def `selectT`[T <: Product](fields: List[Symbol], factory: SelectFactory): SelectSyntax.`selectT`[T] =
    new SelectSyntax.`selectT`[T](fields, factory) {}

  override def `fromT`[T <: Product](elem: QueryElement, tableName: String, factory: SelectFactory): SelectSyntax.`fromT`[T] =
    new SelectSyntax.`fromT`[T](elem, tableName, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `whereT`(elem: QueryElement, predicate: String, factory: SelectFactory): SelectSyntax.`whereT` =
    new SelectSyntax.`whereT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `andT`(elem: QueryElement, predicate: String, factory: SelectFactory): SelectSyntax.`andT` =
    new SelectSyntax.`andT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `orT`(elem: QueryElement, predicate: String, factory: SelectFactory): SelectSyntax.`orT` =
    new SelectSyntax.`orT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }
}
