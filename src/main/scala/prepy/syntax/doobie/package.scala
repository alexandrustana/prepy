package prepy.syntax

import prepy.implicits.Implicits
import prepy.syntax.ast.SQLSyntax

package object doobie extends SQLSyntax with Implicits with DoobieSyntax {}
