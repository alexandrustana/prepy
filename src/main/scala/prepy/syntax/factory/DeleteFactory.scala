package prepy.syntax.factory

import prepy.syntax.ast.{DeleteSyntax, QueryElement}

trait DeleteFactory extends GenericFactory {
  def `deleteT`(tableName: String, factory:         DeleteFactory): DeleteSyntax.`deleteT`
  def `whereT`(elem:       QueryElement, condition: String, factory: DeleteFactory): DeleteSyntax.`whereT`
  def `andT`(elem:         QueryElement, condition: String, factory: DeleteFactory): DeleteSyntax.`andT`
  def `orT`(elem:          QueryElement, condition: String, factory: DeleteFactory): DeleteSyntax.`orT`
}
