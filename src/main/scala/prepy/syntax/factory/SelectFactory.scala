package prepy.syntax.factory

import prepy.syntax.plain.{QueryElement, SelectSyntax}

trait SelectFactory extends GenericFactory {
  def `selectT`[T <: Product](fields: List[Symbol], factory:   SelectFactory): SelectSyntax.`selectT`[T]
  def `fromT`[T <: Product](elem:     QueryElement, tableName: String, factory: SelectFactory): SelectSyntax.`fromT`[T]
  def `whereT`(elem:                  QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`whereT`
  def `andT`(elem:                    QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`andT`
  def `orT`(elem:                     QueryElement, condition: String, factory: SelectFactory): SelectSyntax.`orT`
}
