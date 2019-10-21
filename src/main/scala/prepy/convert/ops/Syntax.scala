package prepy.convert.ops

import prepy.convert.ops.syntax.{DeleteSyntax, SelectSyntax}

trait Syntax extends SelectSyntax with DeleteSyntax {}

object Syntax extends Syntax
