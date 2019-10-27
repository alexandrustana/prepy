package prepy.syntax.factory

import prepy.syntax.ast.{QueryElement, SelectSyntax}

trait SelectFactory extends GenericFactory {
  def `selectT`(fields: List[Symbol], factory:   SelectFactory): SelectSyntax.`selectT`
  def `fromT`(elem:     QueryElement, tableName: String, factory: SelectFactory): SelectSyntax.`fromT`
  def `whereT`(elem:    QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`whereT`
  def `andT`(elem:      QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`andT`
  def `orT`(elem:       QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`orT`
}
