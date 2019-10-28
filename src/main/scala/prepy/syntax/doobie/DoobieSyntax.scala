package prepy.syntax.doobie

import prepy.syntax.doobie.internal.{DoobieDelete, DoobieInsert, DoobieSelect, DoobieUpdate}

trait DoobieSyntax extends DoobieSelect with DoobieDelete with DoobieInsert with DoobieUpdate {}
