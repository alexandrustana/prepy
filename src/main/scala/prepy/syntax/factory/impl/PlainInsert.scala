package prepy.syntax.factory.impl

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.syntax
import prepy.syntax.factory.{DeleteFactory, InsertFactory}
import prepy.syntax.ast
import prepy.syntax.ast.{DeleteSyntax, InsertSyntax, QueryElement}

trait PlainInsert extends InsertFactory {
  override def `insertT`(tableName: String, factory: InsertFactory): InsertSyntax.`insertT` =
    new InsertSyntax.`insertT`(tableName, factory) {}

  override def `valuesT`(elem: QueryElement, fields: List[Symbol], factory: InsertFactory): InsertSyntax.`valuesT` =
    new InsertSyntax.`valuesT`(elem, fields, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }
}
