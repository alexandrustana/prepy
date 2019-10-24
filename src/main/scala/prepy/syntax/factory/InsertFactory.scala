package prepy.syntax.factory

import prepy.syntax.plain.{InsertSyntax, QueryElement}

trait InsertFactory extends GenericFactory {
  def `insertT`(tableName: String, factory: InsertFactory): InsertSyntax.`insertT`
  def `valuesT`(queryElement: QueryElement, fields: List[Symbol], factory: InsertFactory): InsertSyntax.`valuesT`
}
