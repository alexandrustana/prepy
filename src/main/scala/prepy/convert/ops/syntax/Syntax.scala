package prepy.convert.ops.syntax

import prepy.convert.ops.syntax.inner.{DeleteSyntax, SelectSyntax}

trait Syntax extends SelectSyntax with DeleteSyntax {}

object Syntax extends Syntax
