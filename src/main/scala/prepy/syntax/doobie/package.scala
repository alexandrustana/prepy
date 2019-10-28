package prepy.syntax

import prepy.implicits.Implicits
import prepy.syntax.ast.SQLSyntax
import prepy.syntax.doobie.DoobieSyntax

package object doobie extends SQLSyntax with Implicits with DoobieSyntax {}
