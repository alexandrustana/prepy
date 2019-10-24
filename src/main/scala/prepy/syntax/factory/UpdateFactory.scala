package prepy.syntax.factory

import prepy.syntax.ast.{QueryElement, SelectSyntax, UpdateSyntax}

trait UpdateFactory extends GenericFactory {
  def `updateT`(tableName: String, factory: UpdateFactory): UpdateSyntax.`updateT`

  def `setT`(queryElement: QueryElement, fields: List[Symbol], factory: UpdateFactory): UpdateSyntax.`setT`

  def `whereT`(elem: QueryElement, condition: String, factory: UpdateFactory): UpdateSyntax.`whereT`

  def `andT`(elem: QueryElement, condition: String, factory: UpdateFactory): UpdateSyntax.`andT`

  def `orT`(elem: QueryElement, condition: String, factory: UpdateFactory): UpdateSyntax.`orT`
}
