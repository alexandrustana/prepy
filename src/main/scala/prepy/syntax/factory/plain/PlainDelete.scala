package prepy.syntax.factory.plain

import cats.data.Validated
import cats.data.Validated.Valid
import prepy.syntax.factory.DeleteFactory
import prepy.syntax.ast.{DeleteSyntax, QueryElement}

trait PlainDelete extends DeleteFactory {
  override def `deleteT`(tableName: String, factory: DeleteFactory): DeleteSyntax.`deleteT` =
    new DeleteSyntax.`deleteT`(tableName, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `whereT`(elem: QueryElement, predicate: String, factory: DeleteFactory): DeleteSyntax.`whereT` =
    new DeleteSyntax.`whereT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `andT`(elem: QueryElement, predicate: String, factory: DeleteFactory): DeleteSyntax.`andT` =
    new DeleteSyntax.`andT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }

  override def `orT`(elem: QueryElement, predicate: String, factory: DeleteFactory): DeleteSyntax.`orT` =
    new DeleteSyntax.`orT`(elem, predicate, factory) {
      override def apply(): Validated[String, _] = Valid(toString)
    }
}
