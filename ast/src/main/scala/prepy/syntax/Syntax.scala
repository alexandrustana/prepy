package prepy.syntax

import prepy.syntax.plain.AST
import prepy.syntax.implicits.Implicits

trait Syntax extends AST with Implicits {}
