package prepy

import prepy.implicits.Implicits
import prepy.syntax.ast.SQLSyntax

package object syntax extends SQLSyntax with Implicits {}
