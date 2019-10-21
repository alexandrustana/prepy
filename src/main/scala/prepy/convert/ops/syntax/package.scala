package prepy.convert.ops

import prepy.convert.ops.syntax.inner.{DeleteSyntax, InsertSyntax, SelectSyntax}

package object syntax extends SelectSyntax with DeleteSyntax with InsertSyntax {}
