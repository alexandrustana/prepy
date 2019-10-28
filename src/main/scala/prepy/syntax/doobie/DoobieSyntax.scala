package prepy.syntax.doobie

import prepy.syntax.doobie.internal.{DoobieDelete, DoobieInsert, DoobieSelect, DoobieUpdate}

private[doobie] trait DoobieSyntax extends DoobieSelect with DoobieDelete with DoobieInsert with DoobieUpdate {}
